{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE TypeOperators #-}

module Data.Array.Accelerate.Tabular.Util (
  emptyVector, splitKeys,
  rotateLeft, rotateRight
) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Unsafe (undef)

import Data.Array.Accelerate.Tabular.Rep.Snoc

-- | Creates an empty (length-0) vector.
emptyVector :: (Elt e) => Acc (Vector e)
emptyVector = fill (I1 0) undef

splitKeys :: (Elt keys, Elt key)
          => Acc (Vector (keys :.: key))
          -> (Acc (Vector keys), Acc (Vector key))
splitKeys = unzip . map unKey

-- | Rotate a vector left by the specified amount.
-- Negative amounts are also supported
--
-- >>> let vec = fromList (Z:.5) [1..] :: Vector Int
-- >>> vec
-- Vector (Z :. 5) [1, 2, 3, 4, 5]
--
-- >>> run $ rotateLeft 1 (use vec)
-- Vector (Z :. 5) [2, 3, 4, 5, 1]
--
-- >>> run $ rotateLeft (-1) (use vec)
-- Vector (Z :. 5) [5, 1, 2, 3, 4]
--
rotateLeft :: (Elt a) => Exp Int -> Acc (Vector a) -> Acc (Vector a)
rotateLeft r xs = backpermute
  (shape xs)
  (\(I1 i) -> I1 ((i + r) `mod` length xs))
  xs

-- | Rotate a vector right by the specified amount.
-- See also 'rotateLeft'.
--
rotateRight :: (Elt a) => Exp Int -> Acc (Vector a) -> Acc (Vector a)
rotateRight r = rotateLeft (- r)
