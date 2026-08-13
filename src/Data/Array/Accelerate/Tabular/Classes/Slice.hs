{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE GADTs                 #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Data.Array.Accelerate.Tabular.Classes.Slice (
  Slice
, SliceFix (..), pattern Slice_
, SliceDescriptor (..)
, SliceDescriptor' (..)
, SliceResult
, Inner
) where

import Data.Array.Accelerate hiding (Slice, slice, Any(..), Any_)

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Classes.Fold

import Data.Kind

-- | Cut out this dimension by fixing it to a specific key.
--
newtype SliceFix key = Slice key
  deriving (Generic)
  deriving anyclass (Elt)

-- Note:
-- This introduces slight notational overhead compared to Accelerate slices.
-- There, the index can be used directly, without the need for an additional constructor.
-- In this case, it is necessary to avoid overlapping branches in SliceResult:
-- 'Keep' could technically be part of a key.

mkPattern ''SliceFix


-- Every type that is an instance of Rep should also be an instance of Slice.
--
-- However, it may be necessary to keep them apart, for syntactic sugar
-- involving keys not defined using '(:.)'.
--
class (Rep rep key) => Slice rep key where

instance Slice Z Z

class (Elt key, Elt (SliceResult key desc), Elt desc)
  => SliceDescriptor key desc where

  -- | Get a value describing the structure of the descriptor.
  --
  getSliceDescriptor :: Exp desc -> SliceDescriptor' key desc

instance (Eq key) => SliceDescriptor key Z where

  getSliceDescriptor _ = SliceZ

-- The trick with (key ~ key') allows GHC to infer the type of key'.
-- This means a type annotation is not required.

instance ( SliceDescriptor keys desc
         , Eq key
         , key ~ key'
         , Elt (SliceResult (keys :. key) (desc :. SliceFix key'))
         ) => SliceDescriptor (keys :. key) (desc :. SliceFix key') where

  getSliceDescriptor (d ::. Slice_ k) = SliceIndex (getSliceDescriptor d) k

instance (SliceDescriptor keys desc, Eq key)
  => SliceDescriptor (keys :. key) (desc :. Keep) where

  getSliceDescriptor (d ::. _) = SliceKeep (getSliceDescriptor d)

-- | Describes the structure of a slice descriptor in a single data type,
-- to allow pattern matching.
--
data SliceDescriptor' key desc where
  SliceZ    :: SliceDescriptor' key Z
  SliceKeep :: (Elt keys, Elt (SliceResult keys desc), Elt key)
            => SliceDescriptor' keys desc
            -> SliceDescriptor' (keys :. key) (desc :. Keep)
  SliceIndex :: (Elt keys, Elt (SliceResult keys desc), Eq key)
             => SliceDescriptor' keys desc
             -> Exp key
             -> SliceDescriptor' (keys :. key) (desc :. SliceFix key)

-- | The result of applying a given slice descriptor to a type.
--
type SliceResult :: Type -> Type -> Type
type family SliceResult t desc where
  SliceResult t  Z                   = t
  SliceResult t (desc :. Keep)       = SliceResult (Unsnoc t) desc :. Inner t
  SliceResult t (desc :. SliceFix _) = SliceResult (Unsnoc t) desc

type Inner :: Type -> Type
type family Inner t where
  Inner (_ :. t) = t
  Inner t        = t
