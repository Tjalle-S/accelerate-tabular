{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitForAll #-}

module Data.Array.Accelerate.Tabular.Prelude.Cartesian (
  type (++)
, cartesianWith
) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Monoid

import Data.Array.Accelerate.Tabular.Classes.Fold
import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Prelude.Assocs
import Data.Array.Accelerate.Tabular.Prelude.Table

import Data.Array.Accelerate.Tabular.Classes.Key

-- Reference implementation of cartesian product.

-- | Applies a function to all combinations of values in the tables.
-- The corresponding keys are concatenated.
-- 
cartesianWith :: forall rep'' rep' rep key'' key' key c b a
              . ( Key key'
                , Rep rep key, Rep rep' key', Rep rep'' key''
                , Elt a, Elt b, Elt c
                , key'' ~ (key ++ key')
                )
              => (Exp a -> Exp b -> Exp c)
              -> Acc (Table rep key a)
              -> Acc (Table rep' key' b)
              -> Acc (Table rep'' key'' c)
cartesianWith f xs ys =
  let 
      xs' = assocs' xs
      ys' = assocs' ys
      xs'' = replicate (Z_ ::. All_       ::. length ys') xs'
      ys'' = replicate (Z_ ::. length xs' ::. All_)       ys'
  in  createTable' (assumeOrdered xs <> assumeOrdered ys)
        $ flatten
        $ zipWith combine xs'' ys''
  where
    combine (T2 xk xv) (T2 yk yv) =
      let k' = concatKey xk (getKeyR yk)
      in  T2 k' (f xv yv)
