{-# LANGUAGE NoImplicitPrelude    #-}

{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE ConstraintKinds      #-}  

{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DataKinds            #-}

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Array.Accelerate.Tabular.Prelude.Fold (
  fold, fold1
, foldAll, fold1All

, inner, inner1, inner2, inner3

, NotScalarFold

, module Fold
) where

import Data.Array.Accelerate hiding (Scalar, fold, fold1, foldAll, fold1All)
import qualified Data.Array.Accelerate as A

import Data.Array.Accelerate.Data.Lens ()

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Rep
import Data.Array.Accelerate.Tabular.Classes.Fold as Fold
import Data.Array.Accelerate.Tabular.Prelude.Table
import Data.Array.Accelerate.Tabular.Util

import GHC.TypeError
import Control.Applicative (pure)

-- | Reduction of a table of arbitrary dimensionality.
-- The first argument needs to be function that is both associative /and/ commutative.
--
-- Folding can only be performed over 1 or more /innermost/ dimensions.
-- When folding over different dimensions, use 'Data.Array.Tabular.reindex' first to reorder the dimensions.
--
fold :: (NotScalarFold rep, Fold rep key, Elt val, FoldDescriptor rep key desc)
     => desc
     -> (Exp val -> Exp val -> Exp val)
     -> Exp val
     -> Acc (Table rep key val)
     -> Acc (Table (FoldResult rep desc) (FoldResult key desc) val)
fold d f e Table_ { meta_, vals_ } = 
  let T2 met' seg = foldMeta (getDescriptor $ pure d) meta_
  in  Table_ met' $ foldSeg (combineMaybe f) (Just_ e) vals_ seg

-- | Variant of 'fold' that requires each segment being folded over to be
-- non-empty, and does not need a default value.
--
fold1 :: (NotScalarFold rep, Fold rep key, Elt val, FoldDescriptor rep key desc)
      => desc
      -> (Exp val -> Exp val -> Exp val)
      -> Acc (Table rep key val)
      -> Acc (Table (FoldResult rep desc) (FoldResult key desc) val)
fold1 d f Table_ { meta_, vals_ } = 
  let T2 met' seg = foldMeta (getDescriptor $ pure d) meta_
  in  Table_ met' $ fold1Seg (combineMaybe f) vals_ seg

-- | Reduction of a table of arbitrary dimensionality to a single scalar value.
-- The first argument needs to be function that is both associative /and/ commutative.
--
foldAll :: (NotScalarFold rep, Rep rep key, Elt val)
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
fold1All :: (NotScalarFold rep, Rep rep key, Elt val)
        => (Exp val -> Exp val -> Exp val)
        -> Acc (Table rep key val)
        -> Acc (Scalar val)
fold1All f Table_ { vals_ } = 
  let res = A.fold1 (combineMaybe f) vals_
  in  Table_ emptyMeta (flatten res)

-- Ideally, this would be a warning rather than an error, since
-- it will work correctly in all cases, giving the same results as id.
-- However, since custom warnings do not exist, it is an error instead.

-- | Folds should not be executed on scalar tables.
-- 
type NotScalarFold rep = NotScalar (
  Text "Folds on scalar tables (Table Z Z val) perform no work and should be omitted.") rep

-- Common fold descriptors
-- -----------------------

-- | Reduce the innermost dimension of a table.
--
inner, inner1 :: Keep :.: Group
inner  = Keep :.: Group
inner1 = inner

-- | Reduce the 2 innermost dimensions of a table.
--
inner2 :: Keep :.: Group :.: Group
inner2 = inner1 :.: Group

-- | Reduce the 3 innermost dimensions of a table.
--
inner3 :: Keep :.: Group :.: Group :.: Group
inner3 = inner2 :.: Group
