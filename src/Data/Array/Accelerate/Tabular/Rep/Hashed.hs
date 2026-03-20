{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE PatternSynonyms       #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Array.Accelerate.Tabular.Rep.Hashed (
  Hashed
, emptyHashSet, insert, HashSet (..)
) where

import Data.Array.Accelerate

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Rep.Snoc
import Data.Array.Accelerate.Tabular.Util
import Data.Array.Accelerate.Data.Hashable (Hashable(hash))
import Data.Array.Accelerate.Data.Maybe (maybe)

-- | Stores keys in a hash map.
--
data Hashed

instance (Rep rep keys, Hashable key) =>
  Rep (rep :.: Hashed) (keys :.: key) where

  type MetaR (rep :.: Hashed) (keys :.: key) =
    (Meta rep keys, Scalar Int, Vector key)

  emptyMeta = Meta_ $ T3 emptyMeta (unit 0) emptyVector

-- Local utilities.
-- ----------------

-- pattern HashedMeta :: (Arrays (Meta rep keys), Hashable key)
--                        => Acc (Meta rep keys)
--                        -> Acc (Scalar Int)
--                        -> Acc (Vector key)
--                        -> Acc (Meta (rep :.: Hashed) (keys :.: key))
-- pattern HashedMeta { met, width, ks } = Meta_ (T3 met width ks)
-- {-# COMPLETE HashedMeta #-}




data HashSet key = HashSet_ (Vector (Maybe key)) (Scalar Int)
  deriving (Generic, Show)

instance (Elt key) => Arrays (HashSet key)


pattern HashSet :: (Elt key)
                => Acc (Vector (Maybe key))
                -> Acc (Scalar Int)
                -> Acc (HashSet key)
pattern HashSet { keys, width } = Pattern (keys, width)
{-# COMPLETE HashSet #-}

emptyHashSet :: (Elt key) => Exp Int -> Exp Int -> Acc (HashSet key)
emptyHashSet n w =
  let ks = fill (I1 $ n * w) Nothing_
  in  HashSet ks (unit w)

insert :: (Eq key, Hashable key)
       => Acc (Vector (Int, key))
       -> Acc (HashSet key)
       -> (Acc (HashSet key), Exp Int)
insert sks (HashSet { keys, width }) =
  let hks = map (\(T2 b k) -> T3 b (hash k) k) sks
      T3 _ todo' keys' = awhile condition step (T3 (unit 0) hks keys)
  in  (HashSet keys' width, length todo')
  where
    condition :: (Elt key)
              => Acc (Scalar Int, Vector (Int, Int, key), Vector (Maybe key)) -> Acc (Scalar Bool)
    condition (T3 i todo _) = unit $ (the i < the width) && not (null todo)

    step :: (Eq key)
         => Acc (Scalar Int, Vector (Int, Int, key), Vector (Maybe key))
         -> Acc (Scalar Int, Vector (Int, Int, key), Vector (Maybe key))
    step (T3 i todo ks) = 
      let -- Find where to attempt insertion.
          f (T3 b p _) = (p `mod` the width) + (the width * b)
          ps = map f todo

          -- Find collisions with already inserted keys.
          mks = gather ps ks
          (coll, good) = split
            (\x -> isCollision (thd3 $ fst3 x) (thd3 x))
            (zipChecked3 todo ps mks)
          (goodTodo, goodps, _) = unzip3 good
          (_, _, goodks) = unzip3 goodTodo

          -- Insert if no collision.
          ks' = scatter goodps ks (map Just_ goodks)

          -- Find collisions within todo-list:
          -- if key not present in new array, there was a collision.
          mks' = gather goodps ks'
          goodTodo' = map fst $ afst $ filter
            (\x -> isCollision (thd3 $ fst x) (snd x))
            (zipChecked goodTodo mks')

          -- Make new todo-list. 
          (collTodo, _, _) = unzip3 coll
          todo' = map nextInSequence (collTodo ++ goodTodo')

      in  T3 (map (+ 1) i) todo' ks'
      where
        nextInSequence :: (Elt key) => Exp (Int, Int, key) -> Exp (Int, Int, key)
        nextInSequence (T3 b p k) = T3 b (p + 1) k

split :: (Elt a)
      => (Exp a -> Exp Bool)
      -> Acc (Vector a)
      -> (Acc (Vector a), Acc (Vector a))
split f xs = (afst $ filter f xs, afst $ filter (not . f) xs)

isCollision :: (Eq a)
            => Exp a
            -> Exp (Maybe a)
            -> Exp Bool
isCollision k = maybe False_ (/= k)

-- insert' :: Acc (Scalar Int) -> Acc (Vector (Maybe key))
