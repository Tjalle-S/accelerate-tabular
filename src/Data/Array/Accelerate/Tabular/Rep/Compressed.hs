{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeOperators         #-} 
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ConstraintKinds       #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}

module Data.Array.Accelerate.Tabular.Rep.Compressed (
  Compressed
, OrdCompressed
) where

import Data.Array.Accelerate


import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Rep.Snoc
import Data.Array.Accelerate.Tabular.Util

import Data.Type.Equality
import Data.Function (on)

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

  createMeta ks =
    let (ks', is) = splitKeys ks
        (met, perm, flags, n) = createMeta ks'

        is' = gather perm is
        (is'', perm') = unzip $
          segmentedSortBy (compare `on` fst) flags (zipChecked is' perm)

        flags' = headFlagBorders flags is''
        is''' = afst $ compact flags' is''

        n' = unit (length is''')

        -- is''' = uniqSegHead flags is''

        -- seg' = scanl (+) 0 (map boolToInt flags)
        seg' = undefined

        met' = CompressedMeta met seg' is'''
    in  (met', perm', flags', n')

-- Local utilities for compressed representations.
-- -----------------------------------------------

-- | Metadata for compressed representations.
--
type CompressedMetaR rep keys key = (Meta rep keys, Segments Int, Vector key)

type IsCompressed rep r keys key =
  ( Rep rep keys
  , Elt key
  , MetaR (rep :.: r) (keys :.: key) ~ CompressedMetaR rep keys key
  )

pattern CompressedMeta :: (IsCompressed rep r keys key)
                       => Acc (Meta rep keys)
                       -> Acc (Segments Int)
                       -> Acc (Vector key)
                       -> Acc (Meta (rep :.: r) (keys :.: key))
pattern CompressedMeta { meta, seg, keys } = Meta_ (T3 meta seg keys)
{-# COMPLETE CompressedMeta #-}

-- | Creates empty metadata for compressed representations.
--
emptyCompressed :: (IsCompressed rep r keys key)
                => Acc (Meta (rep :.: r) (keys :.: key))
emptyCompressed = let s = fill (I1 2) 0
                  in  CompressedMeta emptyMeta s emptyVector
