{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE TypeOperators     #-}

module Data.Array.Accelerate.Tabular.Util (
  emptyVector
, splitKeys
, segmentedSort, segmentedSortBy
) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Unsafe (undef)

import Data.Array.Accelerate.Tabular.Rep.Snoc
import Data.Array.Accelerate.Tabular.Util.SegmentedSort

-- | Creates an empty (length-0) vector.
emptyVector :: (Elt e) => Acc (Vector e)
emptyVector = fill (I1 0) undef

splitKeys :: (Elt keys, Elt key)
          => Acc (Vector (keys :.: key))
          -> (Acc (Vector keys), Acc (Vector key))
splitKeys = unzip . map unKey
