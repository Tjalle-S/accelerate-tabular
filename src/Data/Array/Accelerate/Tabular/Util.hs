{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RebindableSyntax #-}

module Data.Array.Accelerate.Tabular.Util (
  emptyVector, splitKeys
, rotateLeft, rotateRight
, comparing
, histogram
, combineMaybe
, singleton
, join
, lookupMany
) where

import qualified Prelude as P

import Data.Array.Accelerate
import Data.Array.Accelerate.Unsafe (undef)
import Data.Array.Accelerate.Control.Monad

-- | Creates an empty (length-0) vector.
--
emptyVector :: (Elt e) => Acc (Vector e)
emptyVector = fill (I1 0) undef

-- | Split keys into keys for the parent and current levels.
--
splitKeys :: (Elt keys, Elt key)
          => Acc (Vector (keys :. key))
          -> (Acc (Vector keys), Acc (Vector key))
splitKeys = unzip . map unindex

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
--
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

-- | Deconstruct an index into its tail and head.
--
unindex :: (Elt a, Elt b) => Exp (a :. b) -> Exp (a, b)
unindex (x ::. y) = T2 x y

-- | The 'join' function is the conventional monad join operator. It
-- is used to remove one level of monadic structure, projecting its
-- bound argument into the outer level.
--
--
-- \'@'join' bss@\' can be understood as the @do@ expression
--
-- @
-- do bs <- bss
--    bs
-- @
--
join :: (Monad m, Elt a, Elt (m a), Elt (m (m a))) => Exp (m (m a)) -> Exp (m a)
join = (>>= P.id)

-- | Create a single-element array of any dimensionality.
--
singleton :: (Unit sh, Elt a) => Exp a -> Acc (Array sh a)
singleton = reshape indexUnit . unit

class (Shape sh) => Unit sh where
  indexUnit :: Exp sh

instance Unit Z where
  indexUnit = Z_

instance (Unit sh) => Unit (sh :. Int) where
  indexUnit = indexUnit ::. 1

-- | Lookup multiple keys in an association array.
--
-- This function is non-deterministic when the array contains duplicate keys.
-- In this case, the value associated with one of these keys will be returned.
--
lookupMany :: (Eq a, Elt b)
     => Acc (Vector a)
     -> Acc (Vector (a, b))
     -> Acc (Vector (Maybe b))
lookupMany es vec =
  let ies' = replicate (Z_ ::. length vec ::. All_)      (indexed es)
      vec' = replicate (Z_ ::. All_       ::. length es) vec

      target = fill (shape es) Nothing_

      matches = zipWithChecked f ies' vec'
  in  permuteUnique' target matches
  where
    f (T2 ei ev) (T2 va vb) =
      if ev == va
        then Just_ (T2 ei (Just_ vb))
        else Nothing_
