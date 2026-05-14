{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Benchmarks.Micro.CreateTable where

import Data.Array.Accelerate

import Data.Array.Accelerate.Tabular.Rep
import Data.Array.Accelerate.Tabular.Prelude (Table, createTable, NotScalar)
import Data.Array.Accelerate.Data.Sort.Quick (sortBy)
import Data.Function ( on )
import Data.Array.Accelerate.Tabular.Classes.Rep

table :: (Rep rep key, NotScalar key) => Acc (Vector (key, Float)) -> Acc (Table rep key Float)
table = createTable

array :: (Shape sh, Ord sh) => Acc (Vector (sh, Float)) -> Acc (Array sh Float)
array kvs = let dim  = maximum (map fst kvs)
                vals = map snd $ sortBy (compare `on` fst) kvs
            in reshape (the dim) vals
