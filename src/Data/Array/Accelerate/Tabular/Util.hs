{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Data.Array.Accelerate.Tabular.Util (
  emptyVector, splitKeys
, rotateLeft, rotateRight
, fst3, snd3, thd3
, comparing
, histogram
, combineMaybe
) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Unsafe (undef)

import Data.Array.Accelerate.Tabular.Rep.Snoc

-- | Creates an empty (length-0) vector.
--
emptyVector :: (Elt e) => Acc (Vector e)
emptyVector = fill (I1 0) undef

-- | Split keys into keys for the parent and current levels.
--
splitKeys :: (Elt keys, Elt key)
          => Acc (Vector (keys :.: key))
          -> (Acc (Vector keys), Acc (Vector key))
splitKeys = unzip . map unKey

-- | Rotate a vector left by the specified amount.
-- Negative amounts are also supported
--
-- >>> let vec = fromList (Z:.5) [1..] :: Vector Int
-- >>> vec
-- Vector (Z :. 5) [1,2,3,4,5]
--
-- >>> run $ rotateLeft 1 (use vec)
-- Vector (Z :. 5) [2,3,4,5,1]
--
-- >>> run $ rotateLeft (-1) (use vec)
-- Vector (Z :. 5) [5,1,2,3,4]
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


-- | Extract the first element of a 3-tuple.
--
fst3 :: (Elt a, Elt b, Elt c) => Exp (a, b, c) -> Exp a
fst3 (T3 x _ _) = x

-- | Extract the second element of a 3-tuple.
--
snd3 :: (Elt a, Elt b, Elt c) => Exp (a, b, c) -> Exp b
snd3 (T3 _ y _) = y

-- | Extract the third element of a 3-tuple.
--
thd3 :: (Elt a, Elt b, Elt c) => Exp (a, b, c) -> Exp c
thd3 (T3 _ _ z) = z


-- |
-- > comparing p x y = compare (p x) (p y)
--
-- Useful combinator for use in conjunction with the @xxxBy@ family
-- of functions on containers, for example:
--
-- >   ... sortBy (comparing fst) ...
--
comparing :: (Ord a)
          => (Exp b -> Exp a)
          -> Exp b
          -> Exp b
          -> Exp Ordering
comparing p x y = compare (p x) (p y)

histogram :: Exp DIM1 -> Acc (Vector DIM1) -> Acc (Vector Int)
histogram n ids =
  let zeros = fill n 0
      ones  = fill (shape ids) 1
  in  permute' (+) zeros (map Just_ $ zipChecked ids ones)

-- | Lift a combination function to a combination function on 'Maybe's.
combineMaybe :: Elt a
             => (Exp a -> Exp a -> Exp a)
             -> Exp (Maybe a)
             -> Exp (Maybe a)
             -> Exp (Maybe a)
combineMaybe f mx my = T2 mx my & match \case
  T2 Nothing_  Nothing_  -> Nothing_
  T2 (Just_ x) Nothing_  -> Just_ x
  T2 Nothing_  (Just_ y) -> Just_ y
  T2 (Just_ x) (Just_ y) -> Just_ (f x y)
