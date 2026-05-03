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

module Data.Array.Accelerate.Tabular.Classes.Rep (
  Rep  (..)
, Meta (..)
, pattern Meta_

, AssumeOrd (..)

, isOrderedProxy, isOrderedMeta, IsOrdered
) where

import Prelude (type (~))

import Data.Array.Accelerate

import Control.DeepSeq (NFData)
import Type.Reflection
import Data.Data

-- | Possible representations for tables with a given key type.
--
class (Elt key, Arrays (MetaR rep key), Typeable (Ordered rep)) => Rep rep key where

  -- | The metadata necessary for storing the keys, and associating them with
  -- a vector of values.
  --
  type MetaR rep key


  -- Properties

  -- | Whether or not the representation stores keys in sorted order.
  --
  type Ordered rep :: Bool
  type Ordered rep = False -- Conservatively assume it is not.


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

  -- -- | Variant of 'createMeta' that assumes the input vector is already sorted.
  -- --
  -- orderedCreateMeta :: Acc (Vector key)
  --                   -> Acc (Meta rep key, Vector DIM1, Scalar Int)


  -- | Enumerate all keys in the metadata,
  -- producing a list of the same length as the values array.
  -- 
  -- May contain undefined keys on positions corresponding to a 'Nothing' value.
  --
  enumKeys :: Acc (Meta rep key) -> Acc (Vector key)

  -- zipMeta :: Acc (Meta rep key)
  --         -> Acc (Meta rep key)
  --         -> Acc (Meta rep key, Vector (Maybe ()), Vector (Maybe ()))

  -- filterMeta :: Acc (Meta rep key)
  --            -> Acc ()


newtype Meta rep key = Meta (MetaR rep key)
  deriving (Generic)

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

  type Ordered Z = 'True

  emptyMeta = Meta_ (lift ())

  createMeta _ ks =
    let perm = fill (shape ks) (I1 0)
    in  T3 emptyMeta perm (unit 1)

  enumKeys _ = fill (I1 1) Z_

  -- zipMeta _ _ =
  --   let perm = fill (I1 1) (Just_ $ lift ())
  --   in  T3 emptyMeta perm perm



isOrderedProxy :: forall rep . (Typeable (Ordered rep))
               => Proxy rep
               -> Maybe (Ordered rep :~: True)
isOrderedProxy _ = eqT @(Ordered rep) @True

isOrderedMeta :: forall rep key . (Typeable (Ordered rep))
              => Acc (Meta rep key)
              -> Maybe (Ordered rep :~: True)
isOrderedMeta _ = isOrderedProxy @rep Proxy

type IsOrdered rep = Ordered rep ~ True

data AssumeOrd = AssumeOrdered | NoAssumeOrdered
