{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeOperators         #-} 
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ConstraintKinds       #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Array.Accelerate.Tabular.Rep.Compressed (
  Compressed
, OrdCompressed
) where

import Data.Array.Accelerate

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Rep.Snoc
import Data.Array.Accelerate.Tabular.Util

import Data.Type.Equality

-- | Stores only keys present in the table, in a segmented vector.
--
data Compressed

-- | Like 'Compressed', but maintains keys in sorted order.
--
data OrdCompressed


-- Compressed instances.
-- ---------------------

instance (Rep rep keys, Elt key) =>
  Rep (rep :.: Compressed) (keys :.: key) where

  type MetaR (rep :.: Compressed) (keys :.: key) = CompressedMetaR rep keys key

  emptyMeta = emptyCompressed


-- Ordered compressed instances.
-- -----------------------------

instance (Rep rep keys, Ord key) => 
  Rep (rep :.: OrdCompressed) (keys :.: key) where

  type MetaR (rep :.: OrdCompressed) (keys :.: key) = CompressedMetaR rep keys key

  emptyMeta = emptyCompressed


-- Local utilities for compressed representations.
-- -----------------------------------------------

-- | Metadata for compressed representations.
--
type CompressedMetaR rep keys key = (Meta rep keys, Segments Int, Vector key)

type IsCompressed rep r keys key = (
    Rep rep keys
  , Elt key
  , MetaR (rep :.: r) (keys :.: key) ~ CompressedMetaR rep keys key
  )

pattern CompressedMeta :: (IsCompressed rep r keys key)
                       => Acc (Meta rep keys)
                       -> Acc (Segments Int)
                       -> Acc (Vector key)
                       -> Acc (Meta (rep :.: r) (keys :.: key))
pattern CompressedMeta { met, seg, ks } = Meta_ (T3 met seg ks)
{-# COMPLETE CompressedMeta #-}

-- | Creates empty metadata for compressed representations.
--
emptyCompressed :: (IsCompressed rep r keys key)
                => Acc (Meta (rep :.: r) (keys :.: key))
emptyCompressed = let s = fill (I1 2) 0
                  in  CompressedMeta emptyMeta s emptyVector
