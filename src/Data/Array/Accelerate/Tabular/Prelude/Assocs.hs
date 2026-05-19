{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE NamedFieldPuns #-}

module Data.Array.Accelerate.Tabular.Prelude.Assocs (
  assocs, massocs
, keys, values
) where

import Data.Array.Accelerate

import Data.Array.Accelerate.Data.Maybe
import Data.Array.Accelerate.Data.Functor

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Prelude.Table

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
massocs Table_ { meta_, vals_ } = zipWith
  (\k v -> T2 k <$> v)
  (enumKeys meta_)
  vals_

keys :: (Rep rep key, Elt val)
     => Acc (Table rep key val)
     -> Acc (Vector key)
keys = map fst . assocs

values :: (Rep rep key, Elt val)
       => Acc (Table rep key val)
       -> Acc (Vector val)
values = map snd . assocs
