{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Data.Array.Accelerate.Tabular.Prelude.Slice (
  slice
) where

import Data.Array.Accelerate hiding (Slice, slice, filter)

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Classes.Slice
import Data.Array.Accelerate.Tabular.Prelude.Filter
import Data.Array.Accelerate.Tabular.Prelude.Table

import Lens.Micro
import Data.Array.Accelerate.Data.Lens ()

-- | Index a table with a generalised key.
-- This can be used to cut out entire dimensions from the table.
--
slice :: forall rep' rep key desc val
      .  ( Slice rep key
         , Rep rep' (SliceResult key desc)
         , SliceDescriptor key desc
         , Elt val
         )
      => Exp desc
      -> Acc (Table rep key val)
      -> Acc (Table rep' (SliceResult key desc) val)
slice desc tab = 
  let d = getSliceDescriptor desc
      t = toTransform d
      p = (\k _ -> toPredicate d k)
      kvs = filter' p tab
  in  createTable' (assumeOrdered tab) (map (over _1 t) kvs)

toPredicate :: SliceDescriptor' key desc
            -> Exp key
            -> Exp Bool
toPredicate SliceZ            _          = True_
toPredicate (SliceKeep d)     (k  ::. _) = toPredicate d k
toPredicate (SliceIndex d k') (ks ::. k) = k == k' && toPredicate d ks

toTransform :: SliceDescriptor' key desc
            -> Exp key
            -> Exp (SliceResult key desc)
toTransform SliceZ           k          = k
toTransform (SliceKeep  d)   (ks ::. k) = toTransform d ks ::. k
toTransform (SliceIndex d _) (ks ::. _) = toTransform d ks
