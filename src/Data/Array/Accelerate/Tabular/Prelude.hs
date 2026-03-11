{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeOperators         #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Data.Array.Accelerate.Tabular.Prelude (
  Table (..)
, pattern Table_, meta_, vals_
, emptyTable, createTable
) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Unsafe ( undef )
import Data.Array.Accelerate.Data.Functor

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Classes.Index
import Control.DeepSeq (NFData)
import Data.Array.Accelerate.Tabular.Rep.Snoc

-- | A table, consisting of metadata and values.
--
data Table rep key val = Table {
  -- | The metadata storing the present keys.
  --
  meta :: Meta rep key
  -- | The array of values.
  --
, vals :: Vector val
} deriving (Generic)

instance (NFData (Meta rep key), Elt val, NFData val) =>
  NFData (Table rep key val)

deriving instance (Show (Meta rep key), Elt val, Show val) =>
  Show (Table rep key val)
instance (Arrays (Meta rep key), Elt val) => Arrays (Table rep key val)

pattern Table_ :: (Arrays (Meta rep key), Elt val)
               => Acc (Meta rep key)
               -> Acc (Vector val)
               -> Acc (Table rep key val)
pattern Table_ { meta_, vals_ } = Pattern (meta_, vals_)
{-# COMPLETE Table_ #-}

-- | Create an empty table.
-- 
emptyTable :: (Rep rep key, Elt val) => Acc (Table rep key val)
emptyTable = Table_ {
  meta_ = emptyMeta
, vals_ = fill (I1 0) undef
}

createTable :: (Rep rep key, Elt val)
            => Acc (Vector (key, val))
            -> Acc (Table rep key val) 
createTable kvs = let (ks, vs)       = unzip kvs
                      (met, perm, _, _) = createMeta ks
                  in  Table_ met (gather perm vs)
