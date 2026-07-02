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

, awhile

, innerJoin, leftOuterJoin, rightOuterJoin, fullOuterJoin

, tableDesugar, unsafeTableSugar

, module Tabular.Prelude
) where

import Data.Array.Accelerate hiding (
    Scalar, Vector, the, unit
  , indexed, imap
  , filter
  , awhile
  )
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Data.Maybe

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Classes.Sugar
import Data.Array.Accelerate.Tabular.Prelude.Assocs  as Tabular.Prelude
import Data.Array.Accelerate.Tabular.Prelude.Filter  as Tabular.Prelude
import Data.Array.Accelerate.Tabular.Prelude.Fold    as Tabular.Prelude
import Data.Array.Accelerate.Tabular.Prelude.Index   as Tabular.Prelude
import Data.Array.Accelerate.Tabular.Prelude.Map     as Tabular.Prelude
import Data.Array.Accelerate.Tabular.Prelude.Reindex as Tabular.Prelude
import Data.Array.Accelerate.Tabular.Prelude.Slice   as Tabular.Prelude
import Data.Array.Accelerate.Tabular.Prelude.Table   as Tabular.Prelude
import Data.Array.Accelerate.Tabular.Prelude.Zip     as Tabular.Prelude
import Data.Array.Accelerate.Tabular.Rep.Sugar
import Data.Array.Accelerate.Tabular.Util (singleton)

-- | Construct a single-elemement table from a scalar value.
--
unit :: (Elt val) => Exp val -> Acc (Scalar val)
unit x = Table_ emptyMeta $ singleton (Just_ x)

-- | Extract the element from a single-element table.
--
the :: (Elt val) => Acc (Scalar val) -> Exp val
the = fromJust . assert isJust . (!! 0) . vals_

-- | Pair each element of a table with its key.
--
indexed :: (Rep rep key, Elt val)
        => Acc (Table rep key val)
        -> Acc (Table rep key (key, val))
indexed = imap T2

-- | An array-level 'while' construct. Continue to apply the given function,
-- starting with the initial value, until the test function evaluates to
-- 'False'.
--
awhile :: (Arrays a)
       => (Acc a -> Acc (Scalar Bool)) -- ^ Keep evaluating while this returns 'True'.
       -> (Acc a -> Acc a)             -- ^ Function to apply.
       -> Acc a                        -- ^ Initial value.
       -> Acc a
awhile c = A.awhile (A.unit . the . c)

-- Reference implementations for joins.

innerJoin :: forall rep'' rep' rep key c b a
          . ( Rep rep key, Rep rep' key, Rep rep'' key
            , Elt a, Elt b, Elt c
            )
          => (Exp a -> Exp b -> Exp c)
          -> Acc (Table rep key a)
          -> Acc (Table rep' key b)
          -> Acc (Table rep'' key c)
innerJoin f xs ys =
  let (xks, xvs) = unzip (assocs' xs)
      yvs  = indexMany' ys xks
      kvs = afst
          $ justs
          $ zipWith3 (\k a mb -> T2 k <$> maybe Nothing_ (Just_ . f a) mb)
            xks
            xvs
            yvs
  in  createTable' (assumeOrdered xs) kvs

leftOuterJoin :: forall rep'' rep' rep key c b a
              . ( Rep rep key, Rep rep' key, Rep rep'' key
                , Elt a, Elt b, Elt c
                )
              => (Exp a -> Exp b -> Exp c)
              -> Exp b
              -> Acc (Table rep key a)
              -> Acc (Table rep' key b)
              -> Acc (Table rep'' key c)
leftOuterJoin f d xs ys =
  let (xks, xvs) = unzip (assocs' xs)
      yvs  = indexMany' ys xks
      kvs = zipWith3 (\k a mb -> T2 k $ f a (fromMaybe d mb))
            xks
            xvs
            yvs
  in  createTable' (assumeOrdered xs) kvs

rightOuterJoin :: forall rep'' rep' rep key c b a
               . ( Rep rep key, Rep rep' key, Rep rep'' key
                 , Elt a, Elt b, Elt c
                 )
              => (Exp a -> Exp b -> Exp c)
              -> Exp a
              -> Acc (Table rep key a)
              -> Acc (Table rep' key b) 
              -> Acc (Table rep'' key c)
rightOuterJoin f d = flip $ leftOuterJoin (flip f) d

fullOuterJoin :: forall rep'' rep' rep key c b a
              . ( Rep rep key, Rep rep' key, Rep rep'' key
                , Elt a, Elt b, Elt c
                )
              => (Exp a -> Exp b -> Exp c)
              -> Exp a
              -> Exp b
              -> Acc (Table rep key a)
              -> Acc (Table rep' key b) 
              -> Acc (Table rep'' key c)
fullOuterJoin f dx dy xs ys =
  let (xks, xvs) = unzip (assocs' xs)
      (yks, yvs) = unzip (assocs' ys)

      kvs1 = zipWith3 (\k a mb -> T2 k $ f a (fromMaybe dy mb))
            xks
            xvs
            (indexMany' ys xks)
      kvs2 = zipWith3 (\k b ma -> T2 k $ f (fromMaybe dx ma) b)
            yks
            yvs
            (indexMany' xs yks)
  in  createTable' NoAssumeOrdered (kvs1 ++ kvs2)

-- | Convert a table using syntactic sugar for its keys to the underlying table.
-- 
tableDesugar :: (Rep (SugarR c rep) key, Elt val)
             => Acc (Table (SugarR c rep) key                val)
             -> Acc (Table rep            (Underlying c key) val)
tableDesugar Table_ { meta_, vals_ } = Table_ (toUnderlyingMeta meta_) vals_ 

-- | Convert a table to a variant using syntactic sugar for its keys.
-- This is a potentially unsafe function, as invariants or assumptions
-- associated with the surface key type are not checked.
-- This is true even if such checks are present in the specified conversion.
--
unsafeTableSugar :: (Rep (SugarR c rep) key, Elt val)
                 => Acc (Table rep            (Underlying c key) val)
                 -> Acc (Table (SugarR c rep) key                val)
unsafeTableSugar Table_ { meta_, vals_ } = Table_  (toSurfaceMeta meta_) vals_
