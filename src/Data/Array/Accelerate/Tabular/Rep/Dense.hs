{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE NamedFieldPuns        #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Array.Accelerate.Tabular.Rep.Dense (
  Dense
) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Functor

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Classes.Index
import Data.Array.Accelerate.Tabular.Rep.Snoc
import Data.Array.Accelerate.Tabular.Classes.IndexKey
import Data.Array.Accelerate.Tabular.Util

import Data.Function (on)

-- | All keys on this level are present, and the associated values are 
-- explicitly stored.
--
-- A representation consisting of only 'Dense' levels is equivalent to an array.
--
data Dense


-- Dense instances.
-- ----------------

instance (Rep rep keys, IndexKey key) =>
  Rep (rep :.: Dense) (keys :.: key) where

  type MetaR (rep :.: Dense) (keys :.: key) = (Meta rep keys, Scalar key)

  emptyMeta = DenseMeta emptyMeta (unit $ toKey 0)

  createMeta ks = (DenseMeta met n, perm', flags' ++ fend)
    where
      (ks', is) = splitKeys ks

      (met, perm, flags) = createMeta ks'
      is' = gather perm is
      (is'', perm') = unzip $
        segmentedSortBy (compare `on` fst) flags (zip is' perm)

      n = map (toKey . (+ 1)) (maximum $ map fromKey is)
      fend = fill (I1 1) True_

      flags' = stencil
        (\(l, c, _) -> l /= c)
        (function $ const (-1))
        (map fromKey is'')

instance (Index rep keys, IndexKey key) =>
  Index (rep :.: Dense) (keys :.: key) where

  unsafeToLinearIndex DenseMeta { met, n } (k ::.: i) =
    fromKey (the n) * unsafeToLinearIndex met k + fromKey i

  toLinearIndex       DenseMeta { met, n } (k ::.: i) =
    if i >= toKey 0 && i < the n
      then fmap (\i' -> fromKey (the n) * i' + fromKey i) (toLinearIndex met k)
      else Nothing_

-- Local utilities.
-- ----------------

pattern DenseMeta :: (Arrays (Meta rep keys), Elt key)
              => Acc (Meta rep keys)
              -> Acc (Scalar key)
              -> Acc (Meta (rep :.: Dense) (keys :.: key))
pattern DenseMeta { met, n } = Meta_ (T2 met n)
{-# COMPLETE DenseMeta #-}
