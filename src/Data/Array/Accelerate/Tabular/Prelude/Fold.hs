{-# LANGUAGE NoImplicitPrelude    #-}

{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeOperators        #-}

{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE ScopedTypeVariables  #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Array.Accelerate.Tabular.Prelude.Fold (
  fold, fold1
, foldAll, fold1All

, foldNonCommutative, fold1NonCommutative

, inner, inner1, inner2, inner3

, module Fold
) where

import Data.Array.Accelerate hiding (Scalar, fold, fold1, foldAll, fold1All)
import qualified Data.Array.Accelerate as A

import Data.Array.Accelerate.Data.Lens ()

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Classes.Fold as Fold
import Data.Array.Accelerate.Tabular.Prelude.Table
import Data.Array.Accelerate.Tabular.Util

-- | Reduction of a table of arbitrary dimensionality.
-- The first argument needs to be function that is both associative /and/ commutative.
--
-- Folding can only be performed over 1 or more /innermost/ dimensions.
-- When folding over different dimensions, use 'Data.Array.Tabular.reindex' first to reorder the dimensions.
--
fold :: forall rep key val desc
     .  (Fold rep key, Elt val, FoldDescriptor key desc)
     => desc
     -> (Exp val -> Exp val -> Exp val)
     -> Exp val
     -> Acc (Table rep key val)
     -> Acc (Table (FoldResult rep desc) (FoldResult key desc) val)
fold _ f e Table_ { meta_, vals_ } =
  let (res, prf) = foldMeta (getDescriptor @key @desc) meta_
  in  case prf of
        Dict' -> let T2 met' seg = res
                 in  Table_ met' $ foldSeg (combineMaybe f) (Just_ e) vals_ seg

-- | Variant of 'fold' that requires each segment being folded over to be
-- non-empty, and does not need a default value.
--
fold1 :: forall rep key val desc
      . (Fold rep key, Elt val, FoldDescriptor key desc)
      => desc
      -> (Exp val -> Exp val -> Exp val)
      -> Acc (Table rep key val)
      -> Acc (Table (FoldResult rep desc) (FoldResult key desc) val)
fold1 _ f Table_ { meta_, vals_ } =
  let (res, prf) = foldMeta (getDescriptor @key @desc) meta_
  in  case prf of
        Dict' -> let T2 met' seg = res
                 in  Table_ met' $ fold1Seg (combineMaybe f) vals_ seg

-- | Reduction of a table of arbitrary dimensionality to a single scalar value.
-- The first argument needs to be function that is both associative /and/ commutative.
--
foldAll :: (Rep rep key, Elt val)
        => (Exp val -> Exp val -> Exp val)
        -> Exp val
        -> Acc (Table rep key val)
        -> Acc (Scalar val)
foldAll f e Table_ { vals_ } = 
  let res = A.fold (combineMaybe f) (Just_ e) vals_
  in  Table_ emptyMeta (flatten res)

-- | Variant of 'foldAll' that requires the table to be non-empty
-- and does not need a default value.
--
fold1All :: (Rep rep key, Elt val)
        => (Exp val -> Exp val -> Exp val)
        -> Acc (Table rep key val)
        -> Acc (Scalar val)
fold1All f Table_ { vals_ } = 
  let res = A.fold1 (combineMaybe f) vals_
  in  Table_ emptyMeta (flatten res)

-- | Variant of 'fold' that supports non-commutative combination functions.
-- Can only be used if the order of keys in the table is predictable.
--
foldNonCommutative :: ( IsOrdered rep
                      , Fold rep key
                      , Elt val
                      , FoldDescriptor key desc
                      )
                   => desc
                   -> (Exp val -> Exp val -> Exp val)
                   -> Exp val
                   -> Acc (Table rep key val)
                   -> Acc (Table (FoldResult rep desc) (FoldResult key desc) val)
foldNonCommutative = fold

-- | Variant of 'fold1' that supports non-commutative combination functions.
-- Can only be used if the order of keys in the table is predictable.
--
fold1NonCommutative :: ( IsOrdered rep
                       , Fold rep key
                       , Elt val
                       , FoldDescriptor key desc
                       )
                    => desc
                    -> (Exp val -> Exp val -> Exp val)
                    -> Acc (Table rep key val)
                    -> Acc (Table (FoldResult rep desc) (FoldResult key desc) val)
fold1NonCommutative = fold1

-- Common fold descriptors
-- -----------------------

-- | Reduce the innermost dimension of a table.
--
inner, inner1 :: Keep :. Group
inner  = Keep :. Group
inner1 = inner

-- | Reduce the 2 innermost dimensions of a table.
--
inner2 :: Keep :. Group :. Group
inner2 = inner1 :. Group

-- | Reduce the 3 innermost dimensions of a table.
--
inner3 :: Keep :. Group :. Group :. Group
inner3 = inner2 :. Group
