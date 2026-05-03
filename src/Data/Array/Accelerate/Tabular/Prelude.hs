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
, assocs
, indexed
, filter

, module Tabular.Prelude
) where

import Data.Array.Accelerate hiding (Scalar, the, unit, indexed, (!), imap, filter)
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Data.Maybe

import Data.Array.Accelerate.Tabular.Classes.Rep
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

-- | Return the key-value pairs present in a table.
--
assocs :: (Rep rep key, Elt val)
       => Acc (Table rep key val)
       -> Acc (Vector (key, val))
assocs = afst . justs . massocs

-- | Return the unfiltered key-value pairs present in a table.
--
massocs :: (Rep rep key, Elt val)
        => Acc (Table rep key val)
        -> Acc (Vector (Maybe (key, val)))
massocs Table_ { meta_, vals_ } = A.zipWith
  (\k v -> T2 k <$> v)
  (enumKeys meta_)
  vals_

-- | Pair each element of a table with its key.
--
indexed :: (Rep rep key, Elt val)
        => Acc (Table rep key val)
        -> Acc (Table rep key (key, val))
indexed = imap T2

-- | Return a table containing only the elements that fulfill the given condition.
--
filter :: (NotScalarConstruct rep', Rep rep' key, Rep rep key, Elt val)
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
