{-# LANGUAGE NoImplicitPrelude    #-}

{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE LambdaCase           #-}

{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Array.Accelerate.Tabular.Prelude.Fold (
  fold, fold1
, foldAll, fold1All
) where

import Data.Array.Accelerate hiding (Scalar, fold, fold1, foldAll, fold1All)
import qualified Data.Array.Accelerate as A

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Classes.Fold
import Data.Array.Accelerate.Tabular.Prelude.Table

import Data.Kind
import GHC.TypeLits

-- | Reduction of a table of arbitrary dimensionality.
-- The first argument needs to be function that is both associative /and/ commutative.
fold :: (NotScalar rep, Fold rep key, Elt val)
     => (Exp val -> Exp val -> Exp val)
     -> Exp val
     -> Acc (Table rep key val)
     -> Acc (Table (RepFold rep key) (KeyFold rep key) val)
fold f e Table_ { meta_, vals_ } = 
  let (met', seg) = foldMeta meta_
  in  Table_ met' $ foldSeg (combineMaybe f) (Just_ e) vals_ seg

-- | Variant of 'fold' that requires each segment being folded over to be
-- non-empty, and does not need a default value.
--
fold1 :: (NotScalar rep, Fold rep key, Elt val)
      => (Exp val -> Exp val -> Exp val)
      -> Acc (Table rep key val)
      -> Acc (Table (RepFold rep key) (KeyFold rep key) val)
fold1 f Table_ { meta_, vals_ } = 
  let (met', seg) = foldMeta meta_
  in  Table_ met' $ fold1Seg (combineMaybe f) vals_ seg

-- | Reduction of a table of arbitrary dimensionality to a single scalar value.
-- The first argument needs to be function that is both associative /and/ commutative.
--
foldAll :: (NotScalar rep, Rep rep key, Elt val)
        => (Exp val -> Exp val -> Exp val)
        -> Exp val
        -> Acc (Table rep key val)
        -> Acc (Scalar val)
foldAll f e Table_ { vals_ } = 
  let res = A.fold (combineMaybe f) (Just_ e) vals_
  in  Table_ emptyMeta (flatten res)

-- | Variant of 'foldAll' that requires the table to be non-empty
-- and does not need a default value.
fold1All :: (NotScalar rep, Rep rep key, Elt val)
        => (Exp val -> Exp val -> Exp val)
        -> Acc (Table rep key val)
        -> Acc (Scalar val)
fold1All f Table_ { vals_ } = 
  let res = A.fold1 (combineMaybe f) vals_
  in  Table_ emptyMeta (flatten res)

-- | Lift a combination function to a combination function on 'Maybe's.
combineMaybe :: Elt a
             => (Exp a -> Exp a -> Exp a)
             -> Exp (Maybe a)
             -> Exp (Maybe a)
             -> Exp (Maybe a)
combineMaybe f mx my = T2 mx my & match \case
  T2 Nothing_  Nothing_  -> Nothing_
  T2 (Just_ x) Nothing_  -> Just_ x
  T2 Nothing_  (Just_ y) -> Just_ y
  T2 (Just_ x) (Just_ y) -> Just_ (f x y)

-- Ideally, this would be a warning rather than an error, since
-- it will work correctly in all cases, giving the same results as id.
-- However, since custom warnings do not exist, it is an error instead.

-- | Folds should not be executed on scalar tables.
-- 
type family NotScalar (rep :: Type) :: Constraint where
  NotScalar Z   = TypeError (
    'Text "Folds on scalar tables (Table Z Z val) perform no work and should be omitted.")
  NotScalar rep = ()
