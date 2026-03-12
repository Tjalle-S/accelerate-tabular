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
) where

import Data.Array.Accelerate

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Rep.Snoc
import Data.Array.Accelerate.Tabular.Util
import Data.Array.Accelerate.Data.Hashable (Hashable(hash))
import Data.Array.Accelerate.Data.Maybe (isNothing)

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

insert :: (Hashable key) => Acc (Vector (Int, key))
                         -> Acc (HashSet key)
                         -> Acc (HashSet key)
insert sks (HashSet { keys, width }) =
  let w = the width
      -- start = seg * w

      -- hks = map hash ks
  in  undefined
  where
    condition :: (Elt key) => Acc (Scalar Int, Vector (Int, key), Vector (Maybe key)) -> Acc (Scalar Bool)
    condition (T3 i todo _) = unit $ (the i < the width) && not (null todo)

    step :: (Hashable key) => Acc (Scalar Int, Vector (Int, key), Vector (Maybe key)) -> Acc (Scalar Int, Vector (Int, key), Vector (Maybe key))
    step (T3 i todo ks) = 
      let f (T2 b k) = (hash k `mod` the width) + (the width * b)
          is = map f todo
          (good, coll) = undefined

      in  undefined

split :: (Elt a)
      => (Exp a -> Exp Bool)
      -> Acc (Vector a)
      -> (Acc (Vector a), Acc (Vector a))
split f xs = (afst $ filter f xs, afst $ filter (not . f) xs)

-- insert' :: Acc (Scalar Int) -> Acc (Vector (Maybe key))
