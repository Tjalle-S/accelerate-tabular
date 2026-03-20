{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE NamedFieldPuns        #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}

module Data.Array.Accelerate.Tabular.Rep.Compressed (
  Compressed
, OrdCompressed
) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Sort.Quick

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Rep.Snoc
import Data.Array.Accelerate.Tabular.Util

import Data.Type.Equality
import Data.Array.Accelerate.Data.Semigroup
import Data.Array.Accelerate.Unsafe (undef)

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
    let (ks', is)      = splitKeys ks
        (met, perm, n) = createMeta ks'

        (_, is', perm') = unzip3
          $ sortBy (comparing fst3 <> comparing snd3)
          $ zipChecked3 perm is (enumFromN (shape is) 0)

        histo = histogram (I1 n) perm

        diff = stencil
          (\(l, m, _) -> l /= m)
          (function $ const undef) -- Boundary does not matter.
          is'
        flags = mkHeadFlags (shape is) histo
        flags' = zipWith (||) diff flags -- 1st element of segment always kept.

        T2 is'' n' = compact flags' is'

        segSizes = foldSeg (+) 0 (map boolToInt flags') histo

        perm'' = map (I1 . subtract 1) (scanl1 (+) (map boolToInt flags'))

        met' = CompressedMeta met (scanl1 (+) segSizes) is''
    in  (met', scatter perm' (fill (shape perm') undef) perm'', the n')


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
emptyCompressed = let s = fill (I1 1) 0
                  in  CompressedMeta emptyMeta s emptyVector

histogram :: Exp DIM1 -> Acc (Vector DIM1) -> Acc (Vector Int)
histogram n ids =
  let zeros = fill n 0
      ones  = fill (shape ids) 1
  in  permute' (+) zeros (map Just_ $ zipChecked ids ones)

mkHeadFlags :: Exp DIM1 -> Acc (Segments Int) -> Acc (Vector Bool)
mkHeadFlags n seg =
  let offset = map I1 $ prescanl (+) 0 seg
      falses = fill n False_
      trues  = fill (shape seg) True_
  in  permute' (||) falses (map Just_ $ zipChecked offset trues)
