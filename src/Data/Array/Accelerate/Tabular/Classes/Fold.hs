{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE GADTs                 #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE RankNTypes #-}


module Data.Array.Accelerate.Tabular.Classes.Fold (
  Fold (..)
, FoldDescriptor (..)
, FoldDescriptor' (..)
, Group (..), Keep (..), pattern Keep_
, FoldResult
, Dict' (..), withDict'
) where

import Data.Array.Accelerate

import Data.Array.Accelerate.Tabular.Classes.Rep

import Data.Kind

-- Every type that is an instance of Rep should also be an instance of Fold.
--
-- However, it may be necessary to keep them apart, for syntactic sugar
-- involving keys not defined using '(:.)'.
--

class (Rep rep key, IfSnoc Fold rep key) => Fold rep key where

  -- | Compute the metadata for the table resulting from performing a fold,
  -- and the segment descriptor for performing the fold.
  --
  foldMeta :: FoldDescriptor' key desc
           -> Acc (Meta rep key)
           -> (Acc ( Meta (FoldResult rep desc) (FoldResult key desc)
                  , Segments Int
                  ), Dict' Arrays (Meta (FoldResult rep desc) (FoldResult key desc)))


instance Fold Z Z where

  foldMeta FKeep met = (T2 met (fill (I1 1) $ length (enumKeys met)), Dict')

-- | Describes the result of folding over a representation or key
-- with a given descriptor.
--
type FoldResult :: Type -> Type -> Type
type family FoldResult t desc where
  FoldResult t Keep            = t
  FoldResult t (desc :. Group) = FoldResult (Unsnoc t) desc

-- | Retain this and previous dimensions in their entirety.
--
data Keep  = Keep
  deriving (Generic, Elt)

-- | Fold over this dimension.
--
data Group = Group

pattern Keep_ :: Exp Keep
pattern Keep_ = Pattern ()

-- | Types that can be used to describe the dimensions to fold over.
--
class FoldDescriptor key desc where

  -- | Get the underlying descriptor data type.
  --
  getDescriptor :: FoldDescriptor' key desc

instance FoldDescriptor key Keep where
  getDescriptor = FKeep

instance (FoldDescriptor keys desc)
  => FoldDescriptor (keys :. key) (desc :. Group) where

  getDescriptor = FGroup getDescriptor

-- | The datatype underlying 'FoldDescriptor'.
-- The dimensions to fold over can be determined by pattern matching on this.
--
data FoldDescriptor' key desc where
  FKeep  :: FoldDescriptor' key  Keep
  FGroup :: FoldDescriptor' keys desc
         -> FoldDescriptor' (keys :. key) (desc :. Group)

-- | A value of type @'Dict'' c a@ provides evidence that the constraint @c e@ holds.
data Dict' c a where
  Dict' :: c a => Dict' c a

withDict' :: Dict' c a -> (c a => r) -> r
withDict' d f = case d of
  Dict' -> f
