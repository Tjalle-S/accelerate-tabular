{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Data.Array.Accelerate.Tabular.Classes.Rep (
  Rep  (..)
, Meta (..)
, pattern Meta_
) where

import Data.Array.Accelerate

import Control.DeepSeq (NFData)
import GHC.TypeLits (Nat)

-- | Possible representations for tables with a given key type.
--
class (Elt key, Arrays (MetaR rep key)) => Rep rep key where

  -- | The metadata necessary for storing the keys, and associating them with
  -- a vector of values.
  --
  type MetaR rep key

  -- type Dim rep key :: Nat


  -- Construction.

  -- | Create metadata for an table storing no keys or values.
  --
  emptyMeta :: Acc (Meta rep key)

  -- | Create metadata from a collection of keys.
  --
  -- Also computes a mapping from keys to the position in the metadata
  -- of the next level, and the size of this level.
  --
  createMeta :: Acc (Vector key)
             -> (Acc (Meta rep key), Acc (Vector DIM1), Exp Int)


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

  emptyMeta = Meta_ (lift ())

  createMeta ks =
    let perm = generate (shape ks) (const $ I1 0)
    in  (emptyMeta, perm, 1)

reindex :: (Rep rep' key', Rep rep key)
        => (Exp key -> Exp key')
        -> Acc (Meta rep key)
        -> (Acc (Meta rep' key'), Acc (Vector DIM1))
reindex = undefined
