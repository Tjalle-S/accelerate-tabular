{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Array.Accelerate.Tabular.Classes.Fold (
  Fold (..)
, fold
) where

import Data.Array.Accelerate hiding (fold)

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Table
import Data.Array.Accelerate.Tabular.Util
import Data.Kind
import GHC.TypeLits

class (Rep rep key, Rep (RepFold rep key) (KeyFold rep key)) =>
  Fold rep key where

  -- | The representation for the table resulting from performing a fold.
  --
  type RepFold rep key

  -- | The key resulting for the table resulting from performing a fold.
  --
  type KeyFold rep key

  -- | Compute the metadata for the table resulting from performing a fold,
  -- and the segment descriptor for performing the fold.
  --
  foldMeta :: Acc (Meta rep key)
           -> ( Acc (Meta (RepFold rep key) (KeyFold rep key))
              , Acc (Segments Int)
              )

instance Fold Z Z where

  type RepFold Z Z = Z
  type KeyFold Z Z = Z

  foldMeta _ = (emptyMeta, generate (I1 1) (const 1))

fold :: (NotScalar rep, Fold rep key, Elt val)
     => (Exp val -> Exp val -> Exp val)
     -> Exp val
     -> Acc (Table rep key val)
     -> Acc (Table (RepFold rep key) (KeyFold rep key) val)
fold f e Table_ { meta_, vals_ } = 
  let (met', seg) = foldMeta meta_
  in  Table_ met' $ foldSeg (combineMaybe f) (Just_ e) vals_ seg

-- | Folds cannot be executed on scalar tables.
type family NotScalar (rep :: Type) :: Constraint where
  NotScalar Z   = TypeError (
    'Text "Folds on scalar tables (Table Z Z val) perform no work and should be omitted.")
  NotScalar rep = ()
