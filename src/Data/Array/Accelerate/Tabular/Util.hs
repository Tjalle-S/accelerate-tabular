{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE TypeOperators     #-}

module Data.Array.Accelerate.Tabular.Util (
  emptyVector
, splitKeys
, segmentedSort, segmentedSortBy
, headFlagBorders
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

-- uniq :: (Eq a) => Acc (Vector a) -> Acc (Vector a)
-- uniq xs = let keep = stencil (\(l, m, _) -> l /= m) undefined xs
--           in  afst (compact keep xs)

-- uniqSegHead :: (Eq a) => Acc (Vector Bool) -> Acc (Vector a) -> Acc (Vector a)
-- uniqSegHead = 

headFlagBorders :: (Eq a)
                => Acc (Vector Bool)
                -> Acc (Vector a)
                -> Acc (Vector Bool)
headFlagBorders flags = zipWithChecked (||) (init flags)
                      . stencil isDiff boundary
  where
    isDiff (l, m, _) = l == m
    boundary         = function (const undef)
    -- First head flag always true: boundary does not matter.
