{-# LANGUAGE NoImplicitPrelude #-}

module Data.Array.Accelerate.Tabular.Prelude.Zip (
  zipWith
) where

import Data.Array.Accelerate hiding (zip, zipWith)
import qualified Data.Array.Accelerate as A

import Data.Array.Accelerate.Tabular.Rep
import Data.Array.Accelerate.Tabular.Prelude.Table


-- | Corresponds with a natural join on tables.
--
zipWith :: (Rep rep'' key, Rep rep key, Rep rep' key, Elt a, Elt b, Elt c)
        => (Exp a -> Exp b -> Exp c)
        -> Acc (Table rep   key a)
        -> Acc (Table rep'  key b)
        -> Acc (Table rep'' key c)
zipWith f xs ys = undefined