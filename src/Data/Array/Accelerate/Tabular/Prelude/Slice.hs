{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Array.Accelerate.Tabular.Prelude.Slice (
  slice
) where

import Data.Array.Accelerate hiding (Slice, slice, filter)

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Classes.Slice
import Data.Array.Accelerate.Tabular.Prelude.Filter
import Data.Array.Accelerate.Tabular.Prelude.Table
import Data.Array.Accelerate.Tabular.Prelude.Reindex

-- | Index a table with a generalised key.
-- This can be used to cut out entire dimensions from the table.
--
slice :: forall rep key desc val
      .  ( NotScalar key, NotScalar (SliceResult key desc)
         , Rep rep key
         , SliceDescriptor key desc
         , Elt val
         , Rep (SliceResult rep desc) (SliceResult key desc)
         )
      => Exp desc
      -> Acc (Table rep key val)
      -> Acc (Table (SliceResult rep desc) (SliceResult key desc) val)
slice desc = 
  let d = getSliceDescriptor desc
      t = toTransform d
      p = (\k _ -> toPredicate d k)
  in    reindexUnique @(SliceResult rep desc) t
      . filter @rep p

toPredicate :: SliceDescriptor' key desc
            -> Exp key
            -> Exp Bool
toPredicate SliceZ            _          = True_
toPredicate (SliceKeep d)     (k  ::. _) = toPredicate d k
toPredicate (SliceIndex d k') (ks ::. k) = k == k' && toPredicate d ks

toTransform :: (Elt (SliceResult key desc))
            => SliceDescriptor' key desc
            -> Exp key
            -> Exp (SliceResult key desc)
toTransform SliceZ           _          = Z_
toTransform (SliceKeep  d)   (ks ::. k) = toTransform d ks ::. k
toTransform (SliceIndex d _) (ks ::. _) = toTransform d ks
