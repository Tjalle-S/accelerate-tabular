{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE NamedFieldPuns    #-}

module Data.Array.Accelerate.Tabular.Prelude (
  the, unit
, assocs
, indexed

, module Tabular.Prelude
) where

import Data.Array.Accelerate hiding (Scalar, the, unit, indexed, (!), imap)
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

-- | Construct a single-elemement table from a scalar value.
--
unit :: (Elt val) => Exp val -> Acc (Scalar val)
unit x = Table_ emptyMeta $ generate (I1 1) (const $ Just_ x)

-- | Extract the element from a single-element table.
--
the :: (Elt val) => Acc (Scalar val) -> Exp val
the = (! Z_) -- Assuming a Scalar table always contains exactly one value.

-- | Return the key-value pairs present in a table.
--
assocs :: (Rep rep key, Elt val)
       => Acc (Table rep key val)
       -> Acc (Vector (key, val))
assocs Table_ { meta_, vals_ } = 
  let kvs = A.zipWith (\k v -> T2 k <$> v) (enumKeys meta_) vals_
  in  afst (justs kvs)

-- | Pair each element of a table with its key.
--
indexed :: (Rep rep key, Elt val)
        => Acc (Table rep key val)
        -> Acc (Table rep key (key, val))
indexed = imap T2
