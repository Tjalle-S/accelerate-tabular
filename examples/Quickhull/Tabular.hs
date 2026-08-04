{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Quickhull.Tabular (quickhull) where

-- import qualified Data.Array.Accelerate as A
-- import Data.Array.Accelerate.Tabular.Prelude.Table (vals_)

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Data.Maybe
import Data.Array.Accelerate.Tabular
import Data.Array.Accelerate.Tabular.Prelude (fromArray, toArray, vals_)
import Quickhull.Common

-- Segment, and position within that segment.
type Key = Z :. Int :. Int

type CSR = Z :. Dense :. Compressed
type D1 = Z :. Dense

type State =
  ( Table CSR Key Point,
    Vector Point
  )

quickhull :: Acc (Vector Point) -> Acc (Vector Point)
quickhull =
  asnd
    . awhile (map (> 0) . count . afst) step
    . initialize

initialize :: Acc (Vector Point) -> Acc State
initialize points =
  let line@(T2 leftMost rightMost) =
        the
          $ fold1All (\(T2 min1 max1) (T2 min2 max2) -> T2 (min min1 min2) (max max1 max2))
          $ map (\p -> T2 p p) points

      spoints =
        map
          ( \p ->
              if nonNormalizedDistance line p > 0
                then T2 0 p
                else
                  if nonNormalizedDistance line p < 0
                    then T2 1 p
                    else T2 (-1) p
          )
          points
      remaining = filter @(Z :. Dense) (\_ -> not . (== (-1)) . fst) spoints

      -- 1st dimension not ordered, but uses a scatter anyways.
      -- 2nd dimension order does not matter.
      segs = orderedCreateTable (keyify remaining)

      -- left =
      --   map (T2 0)
      --     $ values
      --     $ filter @(Z :. Compressed)
      --       (\_ p -> nonNormalizedDistance line p > 0.0)
      --       points
      -- right =
      --   map (T2 1)
      --     $ values
      --     $ filter @(Z :. Compressed)
      --       (\_ p -> nonNormalizedDistance line p < 0.0)
      --       points

      -- segs = orderedCreateTable (keyify left ++ keyify right)
      hull =
        fromArray
          $ A.generate
            (I1 2)
            (\(I1 i) -> (i == 0) ? (leftMost, rightMost))
   in T2 segs hull

step :: Acc State -> Acc State
step (T2 segs hull) =
  let length = the $ count hull
      p2s =
        unsafeIndexMany hull
          $ map (\(I1 i) -> I1 $ (i + 1) `mod` length) (keys hull)
      lines = innerJoin @D1 T2 hull p2s

      -- Preferably, we would use a join on the segment index (first key component).
      dist =
        imap
          ( \(I2 s _) p ->
              Just_ $ T2 (nonNormalizedDistance (lines ! (I1 s)) p) p
          )
          segs

      furthest = map (fmap snd) $ fold (Keep :. Group) max Nothing_ dist

      hullPoints = A.map fromJust (vals_ hull)
      furthestPoints = A.map fromJust (vals_ furthest)

      sz = A.map ((? (1, 2)) . isNothing) furthestPoints
      T2 hullIndices size = A.scanl' (+) 0 sz

      target = A.fill (I1 $ A.the size) (T2 0 0)
      target' = A.permuteUnique target (Just_ . I1 . (hullIndices A.!)) hullPoints

      newHull =
        fromArray
          $ A.permuteUnique' target'
          $ A.zipWith (\i mp -> T2 (I1 $ i + 1) <$> mp) hullIndices furthestPoints

      -- Again, a join on segment index would be better.
      leftRight =
        map (\(T2 (I2 s _) p) -> T2 (I1 s) p)
          $ assocs (imap decideLeftRight segs)
      decideLeftRight (I2 s _) p =
        let (T2 p1 p2) = lines ! (I1 s)
            mp = furthest ! (I1 s)
            decide p' =
              if nonNormalizedDistance (T2 p1 p') p > 0
                then T2 0 p
                else
                  if nonNormalizedDistance (T2 p' p2) p > 0
                    then T2 1 p
                    else T2 (-1) p
         in fmap decide mp

      newSegPoints =
        map
          ( \(T2 s (T2 shift p)) ->
              T2 ((hullIndices A.! s) + shift) p
          )
          $ map (\(T2 s mp) -> T2 s $ fromJust mp)
          $ filter (\_ -> isJust . snd) leftRight
      newSeg = orderedCreateTable (keyify newSegPoints)
   in T2 newSeg newHull
  where

keyify :: Acc (Vector (Int, Point)) -> Acc (Vector (Key, Point))
keyify ps =
  fromArray
    $ A.zipWith
      (\(T2 s p) i -> T2 (I2 s i) p)
      (toArray ps)
      (A.enumFromN (Z_ ::. the (count ps)) 0)

-- insert :: (Rep rep key, Elt val)
--        => Acc (Table rep key val)
--        -> Acc (Vector (key, val))
--        -> Acc (Table rep key val)
-- insert tab kvs' = createTable
--                 $ fromArray
--                 $ (toArray $ assocs tab) A.++ (toArray kvs
