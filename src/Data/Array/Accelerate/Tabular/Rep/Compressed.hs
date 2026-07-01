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

module Data.Array.Accelerate.Tabular.Rep.Compressed (Compressed) where

import Data.Array.Accelerate hiding (Slice)
import Data.Array.Accelerate.Data.Sort.Merge

import Data.Array.Accelerate.Tabular.Classes.Rep

import Data.Array.Accelerate.Tabular.Util

import Data.Type.Equality
import Data.Array.Accelerate.Data.Semigroup
import Data.Array.Accelerate.Unsafe (undef)

import Data.Array.Accelerate.Tabular.Classes.Fold

import Lens.Micro
import Lens.Micro.Extras
import Data.Array.Accelerate.Data.Lens ()
import Data.Array.Accelerate.Tabular.Classes.Slice


-- data Compressed

-- | Stores only keys present in the table, in a segmented vector.
-- Keys are maintained in sorted order.
--
data Compressed

-- -- | Like 'Compressed', but may contain duplicate keys.
-- data NonUniqueCompressed

-- Compressed instances.
-- ---------------------

-- instance (Rep rep keys, Elt key) =>
--   Rep (rep :. Compressed) (keys :. key) where

--   type MetaR (rep :. Compressed) (keys :. key) = CompressedMetaR rep keys key

--   emptyMeta = emptyCompressed


-- Ordered compressed instances.
-- -----------------------------

instance (Rep rep keys, Ord key) =>
  Rep (rep :. Compressed) (keys :. key) where

  type MetaR (rep :. Compressed) (keys :. key) = CompressedMetaR rep keys key

  type Ordered (rep :. Compressed) = Ordered rep

  emptyMeta = emptyCompressed

  createMeta o ks =
    let (ks', is)     = splitKeys ks
        T3 met perm n = createMeta o ks'

        (_, is', perm') = case (o, isOrderedMeta met) of
          -- First result is ignored, so undefined is fine here.
          (AssumeOrdered, Just Refl) -> (undefined, is, enumFromN (shape is) 0)
          _                          -> unzip3
            $ sortBy (comparing (view _1) <> comparing (view _2))
            $ zipChecked3 perm is (enumFromN (shape is) 0)

        histo = histogram (I1 $ the n) perm

        diff = stencil
          (\(l, m, _) -> l /= m)
          (function $ const undef) -- Boundary does not matter.
          is'
        flags = mkHeadFlags histo
        flags' = zipWith (||) diff flags -- 1st element of segment always kept.

        T2 is'' n' = compact flags' is'

        segSizes = foldSeg (+) 0 (map boolToInt flags') histo

        perm'' = map (I1 . subtract 1) (scanl1 (+) (map boolToInt flags'))

        met' = CompressedMeta met (scanl1 (+) segSizes) is''
    in  T3 met' (scatter perm' (fill (shape perm') undef) perm'') n'

  enumKeys = enumKeysCompressed


instance (Fold rep keys, Ord key) =>
  Fold (rep :. Compressed) (keys :. key) where

  foldMeta = foldCompressed

instance (Slice rep keys, Ord key) => Slice (rep :. Compressed) (keys :. key)

-- Non-unique compressed instances.
-- --------------------------------

-- instance (Rep rep keys, Ord key) =>
--   Rep (rep :. NonUniqueCompressed) (keys :. key) where

--   type MetaR (rep :. NonUniqueCompressed) (keys :. key) =
--     CompressedMetaR rep keys key

--   emptyMeta = emptyCompressed

--   createMeta o ks = 
--     let (ks', is)     = splitKeys ks
--         T3 met perm n = createMeta o ks'

--         (_, is', perm') = case (o, isOrderedMeta met) of
--           (AssumeOrdered, Just Refl) -> (undefined, is, generate (shape is) id)
--           _                          -> unzip3
--             $ sortBy (comparing $ view _1) 
--             $ zipChecked3 perm is (generate (shape is) id) 


--         histo = histogram (I1 $ the n) perm

--         met' = CompressedMeta met (scanl1 (+) histo) is'
--     in T3 met' perm' (unit $ length is)

--   enumKeys = enumKeysCompressed


-- instance (Fold rep keys, Ord key) =>
--   Fold (rep :. NonUniqueCompressed) (keys :. key) where

--   foldMeta = foldCompressed

-- instance (Slice rep keys, Ord key) => Slice (rep :. NonUniqueCompressed) (keys :. key)

-- Local utilities for compressed representations.
-- -----------------------------------------------

-- | Metadata for compressed representations.
--
type CompressedMetaR rep keys key = (Meta rep keys, Segments Int, Vector key)

type IsCompressed rep r keys key = (
    Rep rep keys
  , Elt key
  , MetaR (rep :. r) (keys :. key) ~ CompressedMetaR rep keys key
  )

pattern CompressedMeta :: (IsCompressed rep r keys key)
                       => Acc (Meta rep keys)
                       -> Acc (Segments Int)
                       -> Acc (Vector key)
                       -> Acc (Meta (rep :. r) (keys :. key))
pattern CompressedMeta { met, seg, ks } = Meta_ (T3 met seg ks)
{-# COMPLETE CompressedMeta #-}

-- | Creates empty metadata for compressed representations.
--
emptyCompressed :: (IsCompressed rep r keys key)
                => Acc (Meta (rep :. r) (keys :. key))
emptyCompressed = let s = fill (I1 1) 0
                  in  CompressedMeta emptyMeta s emptyVector

-- | Folds over compressed metadata.
--
foldCompressed :: ( IsCompressed rep r keys key
                  , Fold rep keys
                  -- , FoldDescriptor (rep :. r) (keys :. key) desc
                  )
               => FoldDescriptor' (keys :. key) desc
               -> Acc (Meta (rep :. r) (keys :. key))
               -> (Acc ( Meta (FoldResult (rep  :. r)   desc)
                             (FoldResult (keys :. key) desc)
                      , Segments Int), Dict' Arrays (Meta (FoldResult (rep :. r) desc) (FoldResult (keys :. key) desc)))
foldCompressed d cmet@CompressedMeta { met, seg } = 
  let seg' = stencil (\(l, m, _) -> m - l) (function $ const 0) seg
  in  case d of
        FKeep       -> (T2 cmet seg', Dict')
        FGroup rest ->
          let (res, prf) = foldMeta rest met
          in  withDict' prf (T2 (afst res) seg', Dict')

enumKeysCompressed :: IsCompressed rep r keys key
                   => Acc (Meta (rep :. r) (keys :. key))
                   -> Acc (Vector (keys :. key))
enumKeysCompressed CompressedMeta { met, seg, ks } =
  expand ((seg' !) . fst) makeKey (indexed $ enumKeys met)
  where
    makeKey (T2 n k) i = let i' = (seg ! n) - (seg' ! n) + i
                         in  k ::. (ks !! i')

    seg' = stencil (\(l, m, _) -> m - l) (function $ const 0) seg

mkHeadFlags :: (HasCallStack) => Acc (Segments Int) -> Acc (Vector Bool)
mkHeadFlags seg
  = init
  $ permute' (||) falses
  $ map (\o -> Just_ (T2 (I1 o) True_)) offset
  where
    T2 offset len = scanl' (+) 0 seg
    falses        = fill (I1 $ the len + 1) False_
