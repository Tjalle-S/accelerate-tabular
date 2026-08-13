{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE ExplicitForAll    #-}

module Data.Array.Accelerate.Tabular.Prelude.Join (
  innerJoin
, leftOuterJoin
, rightOuterJoin
, fullOuterJoin
) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Data.Maybe

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Prelude.Assocs
import Data.Array.Accelerate.Tabular.Prelude.Index
import Data.Array.Accelerate.Tabular.Prelude.Table


-- Reference implementations for joins.

-- | @'innerJoin' f xs ys@ applies function @f@ to each pair of values
-- from @xs@ and @ys@ where the two keys are equal.
-- The resulting table contains only those keys that are present in both @xs@ and @ys@.
--
innerJoin :: forall rep'' rep' rep key c b a
          . ( Rep rep key, Rep rep' key, Rep rep'' key
            , Elt a, Elt b, Elt c
            )
          => (Exp a -> Exp b -> Exp c)
          -> Acc (Table rep key a)
          -> Acc (Table rep' key b)
          -> Acc (Table rep'' key c)
innerJoin f xs ys =
  let (xks, xvs) = unzip (assocs' xs)
      yvs  = indexMany' ys xks
      kvs = afst
          $ justs
          $ zipWith3 (\k a mb -> T2 k <$> maybe Nothing_ (Just_ . f a) mb)
            xks
            xvs
            yvs
  in  createTable' (assumeOrdered xs) kvs

-- | @'innerJoin' f d xs ys@ applies function @f@ to each pair of values
-- from @xs@ and @ys@ where the two keys are equal.
-- The resulting table contains all those keys from @xs@.
-- If a matching key does not exist in @ys@, @d@ is used instead.
--
leftOuterJoin :: forall rep'' rep' rep key c b a
              . ( Rep rep key, Rep rep' key, Rep rep'' key
                , Elt a, Elt b, Elt c
                )
              => (Exp a -> Exp b -> Exp c)
              -> Exp b
              -> Acc (Table rep key a)
              -> Acc (Table rep' key b)
              -> Acc (Table rep'' key c)
leftOuterJoin f d xs ys =
  let (xks, xvs) = unzip (assocs' xs)
      yvs  = indexMany' ys xks
      kvs = zipWith3 (\k a mb -> T2 k $ f a (fromMaybe d mb))
            xks
            xvs
            yvs
  in  createTable' (assumeOrdered xs) kvs

-- | Like 'leftOuterJoin', but takes default values from the first table instead.
--
rightOuterJoin :: forall rep'' rep' rep key c b a
               . ( Rep rep key, Rep rep' key, Rep rep'' key
                 , Elt a, Elt b, Elt c
                 )
              => (Exp a -> Exp b -> Exp c)
              -> Exp a
              -> Acc (Table rep key a)
              -> Acc (Table rep' key b) 
              -> Acc (Table rep'' key c)
rightOuterJoin f d = flip $ leftOuterJoin (flip f) d

-- | Like 'leftOuterJoin', but takes default values for both input tables.
-- This may result in a table with significantly more entries than either
-- of the input tables.
-- 
fullOuterJoin :: forall rep'' rep' rep key c b a
              . ( Rep rep key, Rep rep' key, Rep rep'' key
                , Elt a, Elt b, Elt c
                )
              => (Exp a -> Exp b -> Exp c)
              -> Exp a
              -> Exp b
              -> Acc (Table rep key a)
              -> Acc (Table rep' key b) 
              -> Acc (Table rep'' key c)
fullOuterJoin f dx dy xs ys =
  let (xks, xvs) = unzip (assocs' xs)
      (yks, yvs) = unzip (assocs' ys)

      kvs1 = zipWith3 (\k a mb -> T2 k $ f a (fromMaybe dy mb))
            xks
            xvs
            (indexMany' ys xks)
      kvs2 = zipWith3 (\k b ma -> T2 k $ f (fromMaybe dx ma) b)
            yks
            yvs
            (indexMany' xs yks)
  in  createTable' NoAssumeOrdered (kvs1 ++ kvs2)
