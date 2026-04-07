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
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

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
import Data.Array.Accelerate.Tabular.Classes.Fold
import qualified Prelude as P

data HashStatus = Todo | Done
  deriving (Generic, Elt, Show)

mkPattern ''HashStatus

-- | Stores keys in a hash map.
--
data Hashed

instance (Rep rep keys, Eq key, Hashable key) =>
  Rep (rep :.: Hashed) (keys :.: key) where

  type MetaR (rep :.: Hashed) (keys :.: key) =
    (Meta rep keys, HashSet key)

  emptyMeta = HashedMeta emptyMeta (emptyHashSet 0 0)

  createMeta ks =
    let
      (ks', is) = splitKeys ks
      (met, perm, n) = createMeta ks'

      w = the $ maximum $ histogram (I1 n) perm
      -- TODO: maybe need multiple variations:
      -- - if first level in multidimensional table: many duplicates -> can be smaller
      -- - if e.g. hashed sparse vector: no duplicates -> should be larger to avoid collisions.

      (hset, perm') = insert
        (zipChecked (map unindex1 perm) is)
        (emptyHashSet n w)

      met' = HashedMeta met hset

    in (met', perm', n * w)


instance (Fold rep keys, Eq key, Hashable key) =>
  Fold (rep :.: Hashed) (keys :.: key) where

  type RepFold (rep :.: Hashed) (keys :.: key) = rep
  type KeyFold (rep :.: Hashed) (keys :.: key) = keys

  foldMeta HashedMeta { met, hset } = 
    let seg  = P.snd (foldMeta met)
        len  = sum seg
        seg' = fill (I1 $ the len) (the $ width hset)
    in (met, seg')
  

-- Local utilities.
-- ----------------

pattern HashedMeta :: (Arrays (Meta rep keys), Hashable key)
                       => Acc (Meta rep keys)
                       -> Acc (HashSet key)
                       -> Acc (Meta (rep :.: Hashed) (keys :.: key))
pattern HashedMeta { met, hset } = Meta_ (T2 met hset)
{-# COMPLETE HashedMeta #-}




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
       => Acc (Vector (Bucket, key))
       -> Acc (HashSet key)
       -> (Acc (HashSet key), Acc (Vector DIM1))
insert sks (HashSet { keys, width }) =
  let hks = map (\(T2 b k) -> T4 b (hash k) Todo_ k) sks
      T3 _ work' keys' = awhile condition step (T3 (unit 0) hks keys)
      perm = map (\(T4 b p _ _) -> I1 $ indexOf' b p (the width)) work'
  in  (HashSet keys' width, perm)
  where
    condition :: (Elt key)
              => Acc (Scalar Int, Vector (Bucket, Pos, HashStatus, key), Vector (Maybe key))
              -> Acc (Scalar Bool)
    condition (T3 i work _) = zipWith (&&) (zipWith (<) i width) (any isTodo work)

    step :: (Eq key)
         => Acc (Scalar Int, Vector (Int, Int, HashStatus, key), Vector (Maybe key))
         -> Acc (Scalar Int, Vector (Int, Int, HashStatus, key), Vector (Maybe key))
    step (T3 i work ks) =
      let -- Find where to attempt insertion.
          indexOf (T4 b p _ _) = indexOf' b p (the width)
          ps = map indexOf work

          -- Find collisions with already inserted keys.
          mks = gather ps ks
          flags = zipWithChecked
            (\w@(T4 _ _ _ k) mk -> not (isCollision k mk) && isTodo w)
            work
            mks

          -- Insert if no collision.
          getPermTarget (T4 _ _ _ k) p flag =
            if flag
              then Just_ $ T2 (I1 p) (Just_ k)
              else Nothing_
          ks' = permute' const ks $ zipWithChecked3 getPermTarget work ps flags

          -- Find collisions in todo-list:
          -- if key not present in new-array: need to try again.
          mks' = gather ps ks'
          h w@(T4 b p _ k) mk = mk & match \case
            Just_ k' -> k == k' ? (T4 b p Done_ k, T4 b (nextInSequence p) Todo_ k)
            Nothing_ -> w
          work' = zipWithChecked h work mks'
      in  T3 (map (+ 1) i) work' ks'
      where
        nextInSequence :: Exp Pos -> Exp Pos
        nextInSequence = (+ 1)

isCollision :: (Eq a)
            => Exp a
            -> Exp (Maybe a)
            -> Exp Bool
isCollision k = maybe False_ (/= k)

type Bucket = Int
type Pos = Int
type Width = Int

indexOf' :: Exp Bucket -> Exp Pos -> Exp Width -> Exp Int
indexOf' b p w = (p `mod` w) + (w * b)

isTodo :: (Elt a) => Exp (Bucket, Pos, HashStatus, a) -> Exp Bool
isTodo (T4 _ _ s _) = s & match \case
  Todo_ -> True_
  Done_ -> False_
