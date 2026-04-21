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
) where

import Data.Array.Accelerate

import Control.DeepSeq (NFData)
import Type.Reflection
import Data.Data

-- | Possible representations for tables with a given key type.
--
class (Elt key, Arrays (MetaR rep key)) => Rep rep key where

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
  createMeta :: Acc (Vector key)
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

  type Ordered Z = 'True

  emptyMeta = Meta_ (lift ())

  createMeta ks =
    let perm = generate (shape ks) (const $ I1 0)
    in  T3 emptyMeta perm (unit 1)

  enumKeys _ = flatten (unit Z_)


testOrdered :: forall rep . (Typeable (Ordered rep))
            => Proxy rep
            -> Maybe (Ordered rep :~: True)
testOrdered _ = eqT @(Ordered rep) @True

type IsOrdered rep = Ordered rep ~ True
