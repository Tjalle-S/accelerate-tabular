module Quickhull.Common (
  Point, Line
, nonNormalizedDistance
) where

import Data.Array.Accelerate

-- See module "Quickhull.Accelerate" for source.

type Point = (Double, Double)
type Line = (Point, Point)

-- Computes the distance of a point to a line, which is off by a factor depending on the line.
nonNormalizedDistance :: Exp Line -> Exp Point -> Exp Double
nonNormalizedDistance (T2 (T2 x1 y1) (T2 x2 y2)) (T2 x y) = nx * x + ny * y - c
  where
    nx = y1 - y2
    ny = x2 - x1
    c  = nx * x1 + ny * y1
