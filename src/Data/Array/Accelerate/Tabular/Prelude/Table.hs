{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Array.Accelerate.Tabular.Prelude.Table (
  Table (..)
, Scalar
-- , type NotScalar
, pattern Table_, meta_, vals_
, emptyTable, createTable

, NotScalarConstruct
, NotScalar
) where

import Data.Array.Accelerate hiding (Scalar, unit, the)
import qualified Data.Array.Accelerate as A

import Data.Array.Accelerate.Tabular.Classes.Rep

import Control.DeepSeq (NFData)
import Data.Array.Accelerate.Tabular.Util (emptyVector)
import Data.Kind
import GHC.TypeError

-- | A table, consisting of metadata and values.
--
data Table rep key val = Table {
  -- | The metadata storing the present keys.
  --
  meta :: Meta rep key
  -- | The array of values.
  --
, vals :: Vector (Maybe val)
} deriving (Generic)

deriving instance (Show (Meta rep key), Elt val, Show val) =>
  Show (Table rep key val)
instance (Arrays (Meta rep key), Elt val) => Arrays (Table rep key val)
instance (NFData (Meta rep key), Elt val) => NFData (Table rep key val)

-- | Scalar tables hold a single value.
type Scalar = Table Z Z


pattern Table_ :: (Arrays (Meta rep key), Elt val)
               => Acc (Meta rep key)
               -> Acc (Vector (Maybe val))
               -> Acc (Table rep key val)
pattern Table_ { meta_, vals_ } = Pattern (meta_, vals_)
{-# COMPLETE Table_ #-}

-- | Create an empty table.
-- 
emptyTable :: (NotScalarConstruct rep, Rep rep key, Elt val) => Acc (Table rep key val)
emptyTable = Table_ {
  meta_ = emptyMeta
, vals_ = emptyVector
}

-- | Construct a new table from the given keys and values.
--
createTable :: (NotScalarConstruct rep, Rep rep key, Elt val)
            => Acc (Vector (key, val))
            -> Acc (Table rep key val)
createTable kvs = 
  let (ks, vs)      = unzip kvs
      T3 met perm n = createMeta ks

      perm'  = map unindex1 perm
      target = fill (I1 $ A.the n) Nothing_
      vs'    = map Just_ vs

  in  Table_ met (scatter perm' target vs')

type NotScalarConstruct rep = NotScalar (
       Text "Scalar tables (Table Z Z val) can not be created manually."
  :$$: Text "Use Data.Array.Accelerate.Tabular.unit instead.") rep

-- | Allows constraining to non-scalar tables.
-- Can be used to enforce the assumptions on scalar tables.
--
type family NotScalar (msg :: ErrorMessage) rep :: Constraint where
  NotScalar msg Z         = TypeError msg
  NotScalar _   (_ :. _) = ()
