{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DefaultSignatures #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Data.Array.Accelerate.Tabular.Classes.Rep (
  module Data.Array.Accelerate.Tabular.Classes.Rep
) where

import Data.Array.Accelerate

import Control.DeepSeq (NFData)
import Data.Typeable
import Data.Array.Accelerate.Tabular.Rep.GenProperties
import Data.Kind (Constraint, Type)
import Data.Type.Equality
import qualified Prelude as P

-- | Possible representations for tables with a given key type.
--
class (Eq key, Arrays (MetaR rep key), Typeable (Ordered rep), Typeable (FastIndex rep)) => Rep rep key where

  -- | The metadata necessary for storing the keys, and associating them with
  -- a vector of values.
  --
  type MetaR rep key


  -- Properties

  -- | Whether or not the representation stores keys in sorted order.
  --
  type Ordered rep :: Bool
  type Ordered rep = False -- Conservatively assume it is not.

  -- | Whether or not the representation supports efficient indexing.
  --
  type FastIndex rep :: Bool
  type FastIndex rep = False

  getIndexConstraint :: Proxy rep
                     -> Proxy key
                     -> MaybeDict (FastIndex rep) (Index rep key)
  default getIndexConstraint :: (FastIndex rep ~ False)
                             => Proxy rep
                             -> Proxy key
                             -> MaybeDict (FastIndex rep) (Index rep key)
  getIndexConstraint _ _ = NoDict

  -- Construction

  -- | Create metadata for an table storing no keys or values.
  --
  emptyMeta :: Acc (Meta rep key)

  -- | Create metadata from a collection of keys.
  --
  -- Also computes a mapping from keys to the position in the metadata
  -- of the next level, and the size of this level.
  --
  createMeta :: AssumeOrd
             -> Acc (Vector key)
             -> Acc (Meta rep key, Vector DIM1, Scalar Int)

  -- | Enumerate all keys in the metadata,
  -- producing a list of the same length as the values array.
  -- 
  -- May contain undefined keys on positions corresponding to a 'Nothing' value.
  --
  enumKeys :: Acc (Meta rep key) -> Acc (Vector key)

-- | The metadata necessary for storing the keys, and associating them with
-- a vector of values.
--
newtype Meta rep key = Meta (MetaR rep key)
  deriving (Generic)
-- Note: this newtype is necessary because an associated type is not injective.
-- Therefore, functions like 'emptyMeta' would't typecheck otherwise.
-- However, using a data family instead is impossible because we can't 
-- make instances for Lift/Unlift/Array, nor pattern synonyms.
-- That would make using them in embedded code impossible

deriving instance Show   (MetaR rep key) => Show   (Meta rep key)
instance          Arrays (MetaR rep key) => Arrays (Meta rep key)
instance          NFData (MetaR rep key) => NFData (Meta rep key)

{-# COMPLETE Meta_ #-}
pattern Meta_ :: Arrays (MetaR rep key)
              => Acc (MetaR rep key)
              -> Acc (Meta rep key)
pattern Meta_ dat = Pattern dat

instance Rep Z Z where

  type MetaR Z Z = ()

  type Ordered   Z = True
  type FastIndex Z = True

  getIndexConstraint _ _ = Dict

  emptyMeta = Meta_ (lift ())

  createMeta _ ks =
    let perm = fill (shape ks) (I1 0)
    in  T3 emptyMeta perm (unit 1)

  enumKeys _ = fill (I1 1) Z_


-- | Whether or not a function may assume the input is already sorted.
--
data AssumeOrd = NoAssumeOrdered | AssumeOrdered
  deriving (P.Eq, P.Ord)

instance P.Semigroup AssumeOrd where
  AssumeOrdered <> AssumeOrdered = AssumeOrdered
  _             <> _             = NoAssumeOrdered

instance P.Monoid AssumeOrd where
  mempty = NoAssumeOrdered


-- | Contains an explicit dictionary if the given boolean is 'True'.
data MaybeDict :: Bool -> Constraint -> Type where
  NoDict ::      MaybeDict False a
  Dict   :: a => MaybeDict True  a

-- | Representations that support efficient indexing.
--
class (Rep rep key, FastIndex rep ~ True)
  => Index rep key where
  
  -- | Convert a key into an index into the value array.
  -- No additional checks are performed.
  unsafeToLinearIndex :: Acc (Meta rep key)
                      -> Exp key
                      -> Exp Int

  -- | Convert a key into an index into the value array.
  -- If the key is not present, returns 'Nothing'.
  toLinearIndex :: Acc (Meta rep key)
                -> Exp key
                -> Exp (Maybe Int)

    -- | Like 'unsafeToLinearIndex', but converts multiple keys in parallel.`
  unsafeToLinearIndices :: Acc (Meta rep key)
                        -> Acc (Vector key)
                        -> Acc (Vector Int)

  -- | Like 'toLinearIndex', but converts multiple keys in parallel.
  toLinearIndices :: Acc (Meta rep key)
                  -> Acc (Vector key)
                  -> Acc (Vector (Maybe Int))

instance Index Z Z where
  unsafeToLinearIndex _ _ = 0
  toLinearIndex       _ _ = Just_ 0

  unsafeToLinearIndices _ ks = fill (shape ks) 0
  toLinearIndices       _ ks = fill (shape ks) (Just_ 0)

-- | If the given key has the shape @(keys ':.' key)@,
-- the constraint should hold for @'Unsnoc' rep@ and @keys@ (as well).
--
type IfSnoc :: (Type -> Type -> Constraint) -> Type -> Type -> Constraint
type family IfSnoc c rep key where
  IfSnoc c rep (keys :. key) = c (Unsnoc rep) keys
  IfSnoc _ _   _             = ()
  
-- | The type with the innermost dimension removed, if present.
-- 
-- >>> :t Unsnoc (Z :. Int :. Int)
-- Z :. Int
type Unsnoc :: Type -> Type
type family Unsnoc t where
  Unsnoc (ks :. k) = ks
  Unsnoc a         = a

-- Generate helper functions and constraints for checking properties.
genProperties [''Ordered]

-- This orphan instance might be better of in Accelerate itself.
-- However, this would be slightly superfluous, since only integers are used there.
-- instance Eq (sh :. Int) is already present.
instance {-# INCOHERENT #-} (Eq tail, Eq head) => Eq (tail :. head) where
  x == y = indexHead x == indexHead y &&! indexTail x == indexTail y
  x /= y = indexHead x /= indexHead y ||! indexTail x /= indexTail y
