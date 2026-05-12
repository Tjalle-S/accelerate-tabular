{-# LANGUAGE NoImplicitPrelude #-}

module Data.Array.Accelerate.Tabular.Prelude.Zip (
  -- zip, zipWith
) where

import Data.Array.Accelerate hiding (zip, zipWith)
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.Control.Monad
import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Unsafe

import Data.Array.Accelerate.Tabular.Rep
import Data.Array.Accelerate.Tabular.Prelude.Table
import Data.Array.Accelerate.Data.Maybe (justs)

-- | Corresponds with a natural join on tables.
--
-- zipWith :: (Rep rep key, Elt a, Elt b, Elt c)
--         => (Exp a -> Exp b -> Exp c)
--         -> Acc (Table rep key a)
--         -> Acc (Table rep key b)
--         -> Acc (Table rep key c)
-- zipWith f xs ys = case 
  -- let T3 met' pxs pys = zipMeta (meta_ xs) (meta_ ys)
  --     xs' = afst $ justs $ A.zipWith ($>) pxs (vals_ xs)
  --     ys' = afst $ justs $ A.zipWith ($>) pys (vals_ ys)
  --     vals' = A.zipWith (liftM2 f) xs' ys'
  -- in  Table_ met' vals'
  -- where
  --   permuteVals perm vs = permuteUnique'
  --     (fill (shape perm) undef)
  --     (A.zipWith (fmap . flip T2) vs perm)

-- zip :: (Rep rep key, Elt val, Elt val')
--     => Acc (Table rep key val)
--     -> Acc (Table rep key val')
--     -> Acc (Table rep key (val, val'))
-- zip = zipWith T2
