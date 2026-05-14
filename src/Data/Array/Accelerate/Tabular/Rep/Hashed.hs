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

import Data.Array.Accelerate.Tabular.Util hiding (lookup)
import Data.Array.Accelerate.Data.Hashable (Hashable(hash))
import Data.Array.Accelerate.Data.Maybe
import Data.Array.Accelerate.Tabular.Classes.Fold
import Data.Array.Accelerate.Unsafe (undef)

import Data.Typeable
import Data.Array.Accelerate.Control.Monad

data HashStatus = Todo | Done
  deriving (Generic, Elt, Show)

mkPattern ''HashStatus

-- | Stores keys in a hash map.
--
data Hashed

instance (Rep rep keys, Eq key, Hashable key) =>
  Rep (rep :. Hashed) (keys :. key) where

  type MetaR (rep :. Hashed) (keys :. key) =
    (Meta rep keys, HashSet key)

  -- In this particular case, indexing is not particularly fast.
  -- However, in general, any hashing-based container should support fast indexing.
  type FastIndex (rep :. Hashed) = FastIndex rep

  getIndexConstraint _ _ =
    case getIndexConstraint (Proxy @rep) (Proxy @keys) of
      NoDict -> NoDict
      Dict   -> Dict


  emptyMeta = HashedMeta emptyMeta (emptyHashSet 0 0)

  createMeta o ks =
    let
      (ks', is) = splitKeys ks
      T3 met perm n = createMeta o ks'

      w = the $ maximum $ histogram (I1 $ the n) perm
      -- TODO: maybe need multiple variations:
      -- - if first level in multidimensional table: many duplicates -> can be smaller
      -- - if e.g. hashed sparse vector: no duplicates -> should be larger to avoid collisions.

      T2 hset perm' = insert
        (zipChecked (map unindex1 perm) is)
        (emptyHashSet (the n) w)

      met' = HashedMeta met hset

    in T3 met' perm' (zipWith (*) n (unit w))


  enumKeys HashedMeta { met, hset } = expand
    (const $ the $ width hset)
    (\k i -> maybe undef (k ::.) (keys hset !! i))
    (enumKeys met)


instance (Index rep keys, Eq key, Hashable key) =>
  (Index (rep :. Hashed) (keys :. key)) where

  -- Parallel reads do not require a primive (see the implementation of 'gather'), so lookup can be scalar while-loop.
  -- This will give roughly the same execution pattern as manually masking which entries are done.
  toLinearIndices HashedMeta { met, hset } keys =
    let (ks, is) = splitKeys keys
        mbs      = toLinearIndices met ks
    in zipWithChecked (\k mb -> lookupHash hset k =<< mb) is mbs
  
  unsafeToLinearIndices HashedMeta { met, hset } keys =
    let (ks, is) = splitKeys keys
        bs       = map fromJust $ toLinearIndices met ks
    in map fromJust $ zipWithChecked (lookupHash hset) is bs

  toLinearIndex HashedMeta { met, hset } key = 
    let T2 k i = unindex key
        mb     = toLinearIndex met k
    in  lookupHash hset i =<< mb

  unsafeToLinearIndex HashedMeta { met, hset } key =
    let T2 k i = unindex key
        b      = unsafeToLinearIndex met k
    in  fromJust $ lookupHash hset i b



instance (Fold rep keys, Eq key, Hashable key) =>
  Fold (rep :. Hashed) (keys :. key) where

  foldMeta d hmet@HashedMeta { met, hset } =
    let n = the $ width hset
    in  case d of
          FoldKeep ->
            let T2 _ seg = foldMeta FoldKeep met
                len      = sum seg
                seg'     = fill (I1 $ the len) n
            in  T2 hmet seg'
          FoldGroup FoldKeep ->
            let T2 met' seg = foldMeta FoldKeep met
                len         = sum seg
                seg'        = fill (I1 $ the len) n
            in  T2 met' seg'
          FoldGroup rest ->
            let T2 met' seg = foldMeta rest met
                seg'        = map (* n) seg
            in  T2 met' seg'
  

-- Local utilities.
-- ----------------

pattern HashedMeta :: (Arrays (Meta rep keys), Hashable key)
                       => Acc (Meta rep keys)
                       -> Acc (HashSet key)
                       -> Acc (Meta (rep :. Hashed) (keys :. key))
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

lookupHash :: (HasCallStack, Eq key, Hashable key)
       => Acc (HashSet key)
       -> Exp key
       -> Exp Bucket
       -> Exp (Maybe Int)
lookupHash HashSet { keys, width } k b =
  let start = T3 0 (Just_ $ hash k) False_
      T3 _ mp done = while condition step start
  in  if done
        then mp
        else Nothing_
  where
    condition (T3 i mp _) = i < the width && isJust mp

    step (T3 i mp _) = 
      let p  = fromJust mp -- Safe: 'condition' checks isJust.
          p' = indexOf' b p (the width)
      in  (keys !! p') & match \case
        Nothing_ -> T3 undef Nothing_ True_ -- Empty entry: key not present.
        Just_ k' -> if k' == k
                      -- Key found: stop here.
                      then T3 (the width) (Just_ p') True_
                      -- Other key found: keep looking.
                      else T3 (i + 1) (Just_ $ nextInSequence p) False_


insert :: (Eq key, Hashable key)
       => Acc (Vector (Bucket, key))
       -> Acc (HashSet key)
       -> Acc (HashSet key, Vector DIM1)
insert sks (HashSet { keys, width }) =
  let hks = map (\(T2 b k) -> T4 b (hash k) Todo_ k) sks
      T3 _ work' keys' = awhile condition step (T3 (unit 0) hks keys)
      perm = map (\(T4 b p _ _) -> I1 $ indexOf' b p (the width)) work'
  in  T2 (HashSet keys' width) perm
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
