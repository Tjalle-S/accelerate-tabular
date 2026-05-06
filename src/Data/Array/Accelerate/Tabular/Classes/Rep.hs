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

module Data.Array.Accelerate.Tabular.Classes.Rep (
  module Data.Array.Accelerate.Tabular.Classes.Rep
) where

import Data.Array.Accelerate

import Control.DeepSeq (NFData)
import Type.Reflection
import Data.Array.Accelerate.Tabular.Rep.GenProperties (genProperties)

-- | Possible representations for tables with a given key type.
--
class (Elt key, Arrays (MetaR rep key), Typeable (Ordered rep), Typeable (FastIndex rep)) => Rep rep key where

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

  type Ordered Z = True

  emptyMeta = Meta_ (lift ())

  createMeta _ ks =
    let perm = fill (shape ks) (I1 0)
    in  T3 emptyMeta perm (unit 1)

  enumKeys _ = fill (I1 1) Z_

-- | Whether or not a function may assume the input is already sorted.
--
data AssumeOrd = AssumeOrdered | NoAssumeOrdered

genProperties [''Ordered, ''FastIndex]
