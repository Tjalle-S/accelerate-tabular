{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}

module Data.Array.Accelerate.Tabular.Prelude (
  the, unit
, indexed
, filter

, innerjoin, leftouterjoin, rightouterjoin, fullouterjoin

, module Tabular.Prelude
) where

import Data.Array.Accelerate hiding (Scalar, the, unit, indexed, (!), imap, filter)
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Data.Maybe

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Prelude.Assocs  as Tabular.Prelude
import Data.Array.Accelerate.Tabular.Prelude.Fold    as Tabular.Prelude
import Data.Array.Accelerate.Tabular.Prelude.Index   as Tabular.Prelude
import Data.Array.Accelerate.Tabular.Prelude.Map     as Tabular.Prelude
import Data.Array.Accelerate.Tabular.Prelude.Reindex as Tabular.Prelude
import Data.Array.Accelerate.Tabular.Prelude.Table   as Tabular.Prelude
import Data.Array.Accelerate.Tabular.Prelude.Zip     as Tabular.Prelude
import Data.Data
import Data.Array.Accelerate.Tabular.Util (singleton)

-- | Construct a single-elemement table from a scalar value.
--
unit :: (Elt val) => Exp val -> Acc (Scalar val)
unit x = Table_ emptyMeta $ singleton (Just_ x)

-- | Extract the element from a single-element table.
--
the :: (Elt val) => Acc (Scalar val) -> Exp val
the = (! Z_) -- Assuming a Scalar table always contains exactly one value.

-- | Pair each element of a table with its key.
--
indexed :: (Rep rep key, Elt val)
        => Acc (Table rep key val)
        -> Acc (Table rep key (key, val))
indexed = imap T2

-- | Return a table containing only the elements that fulfill the given condition.
--
filter :: (NotScalar key, Rep rep' key, Rep rep key, Elt val)
       => (Exp key -> Exp val -> Exp Bool)
       -> Acc (Table rep  key val)
       -> Acc (Table rep' key val)
filter p tab =
  let kvs   = massocs tab
  in  createTable' (isOrdered tab)
        $ afst
        $ compact
          (A.map (maybe False_ $ uncurry p) kvs)
          (A.map fromJust                   kvs)

isOrdered :: forall rep key val . (Rep rep key)
          => Acc (Table rep key val)
          -> AssumeOrd
isOrdered _ = case isOrderedProxy @rep Proxy of
  Nothing   -> NoAssumeOrdered
  Just Refl -> AssumeOrdered


-- Reference implementations for joins.

innerjoin :: forall rep'' rep' rep key c b a
          . ( NotScalar key
            , Rep rep key, Rep rep' key, Rep rep'' key
            , Elt a, Elt b, Elt c
            )
          => (Exp a -> Exp b -> Exp c)
          -> Acc (Table rep key a)
          -> Acc (Table rep' key b)
          -> Acc (Table rep'' key c)
innerjoin f xs ys =
  let (xks, xvs) = unzip (assocs xs)
      yvs  = indexMany ys xks
      kvs = afst
          $ justs
          $ zipWith3 (\k a mb -> T2 k <$> maybe Nothing_ (Just_ . f a) mb)
            xks
            xvs
            yvs
  in  createTable kvs

leftouterjoin :: forall rep'' rep' rep key c b a
              . ( NotScalar key
                , Rep rep key, Rep rep' key, Rep rep'' key
                , Elt a, Elt b, Elt c
                )
              => (Exp a -> Exp b -> Exp c)
              -> Exp b
              -> Acc (Table rep key a)
              -> Acc (Table rep' key b)
              -> Acc (Table rep'' key c)
leftouterjoin f d xs ys =
  let (xks, xvs) = unzip (assocs xs)
      yvs  = indexMany ys xks
      kvs = zipWith3 (\k a mb -> T2 k $ f a (fromMaybe d mb))
            xks
            xvs
            yvs
  in  createTable kvs

rightouterjoin :: forall rep'' rep' rep key c b a
               . ( NotScalar key
                 , Rep rep key, Rep rep' key, Rep rep'' key
                 , Elt a, Elt b, Elt c
                 )
              => (Exp a -> Exp b -> Exp c)
              -> Exp a
              -> Acc (Table rep key a)
              -> Acc (Table rep' key b) 
              -> Acc (Table rep'' key c)
rightouterjoin f d = flip $ leftouterjoin (flip f) d

fullouterjoin :: forall rep'' rep' rep key c b a
              . ( NotScalar key
                , Rep rep key, Rep rep' key, Rep rep'' key
                , Elt a, Elt b, Elt c
                )
              => (Exp a -> Exp b -> Exp c)
              -> Exp a
              -> Exp b
              -> Acc (Table rep key a)
              -> Acc (Table rep' key b) 
              -> Acc (Table rep'' key c)
fullouterjoin f dx dy xs ys =
  let (xks, xvs) = unzip (assocs xs)
      (yks, yvs) = unzip (assocs ys)

      kvs1 = zipWith3 (\k a mb -> T2 k $ f a (fromMaybe dy mb))
            xks
            xvs
            (indexMany ys xks)
      kvs2 = zipWith3 (\k b ma -> T2 k $ f (fromMaybe dx ma) b)
            yks
            yvs
            (indexMany xs yks)
  in  createTable (kvs1 ++ kvs2)
