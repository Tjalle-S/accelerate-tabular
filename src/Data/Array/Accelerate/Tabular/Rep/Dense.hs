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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Array.Accelerate.Tabular.Rep.Dense (
  Dense
) where

import qualified Prelude as P

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Functor

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Classes.Index
import Data.Array.Accelerate.Tabular.Rep.Snoc
import Data.Array.Accelerate.Tabular.Classes.IndexKey
import Data.Array.Accelerate.Tabular.Util
import Data.Array.Accelerate.Tabular.Classes.Fold
import Data.Proxy


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
  
  type MetaR (rep :.: Dense) (keys :.: key) = (Meta rep keys, Scalar Int)

  emptyMeta = DenseMeta emptyMeta (unit $ toKey 0)

  createMeta ks =
    let (ks', is)      = splitKeys ks
        (met, perm, n) = createMeta ks'

        n'   = 1 + fromKey (the $ maximum is)
        met' = DenseMeta met (unit n')

        perm' = zipWith (\(I1 p) i -> I1 $ toKey $ p * n' + fromKey i) perm is
    in  (met', perm', n * n')


instance (Index rep keys, IndexKey key) =>
  Index (rep :.: Dense) (keys :.: key) where

  unsafeToLinearIndex DenseMeta { met, n } (k ::.: i) =
    fromKey (the n) * unsafeToLinearIndex met k + fromKey i

  toLinearIndex       DenseMeta { met, n } (k ::.: i) =
    if i >= toKey 0 && fromKey i < the n
      then fmap (\i' -> fromKey (the n) * i' + fromKey i) (toLinearIndex met k)
      else Nothing_


instance (Fold rep keys, IndexKey key) => Fold (rep :.: Dense) (keys :.: key) where
  type RepFold (rep :.: Dense) (keys :.: key) = rep
  type KeyFold (rep :.: Dense) (keys :.: key) = keys

  foldMeta DenseMeta { met, n } = 
    let seg  = P.snd (foldMeta met)
        len  = sum seg
        seg' = fill (I1 $ the len) (the n)
    in (met, seg')

instance (Fold' rep keys, IndexKey key) => Fold' (rep :.: Dense) (keys :.: key) where

  type Dim (rep :.: Dense) = Succ (Dim rep)

  type instance FoldRepResult (rep :.: Dense) Zero = rep :.: Dense
  type instance FoldRepResult (rep :.: Dense) (Succ k) = FoldRepResult rep k

  type instance FoldKeyResult (keys :.: key) Zero = keys :.: key
  type instance FoldKeyResult (keys :.: key) (Succ k) = FoldKeyResult keys k

  foldMeta' k dmet@DenseMeta { met, n } =
    case k of
      SSucc (SSucc k') ->
        let T2 met' seg  = foldMeta' (SSucc k') met
            seg'         = map (* the n) seg
        in  T2 met' seg'
      SSucc SZero      ->
        let T2 met' seg  = foldMeta' SZero met
            len  = sum seg
            seg' = fill (I1 $ the len) (the n)
        in T2 met' seg'
      SZero            ->
        let seg  = asnd $ foldMeta' SZero met
            len  = sum seg
            seg' = fill (I1 $ the len) (the n)
        in T2 dmet seg'
      

-- Local utilities.
-- ----------------

pattern DenseMeta :: (Arrays (Meta rep keys), Elt key)
              => Acc (Meta rep keys)
              -> Acc (Scalar Int)
              -> Acc (Meta (rep :.: Dense) (keys :.: key))
pattern DenseMeta { met, n } = Meta_ (T2 met n)
{-# COMPLETE DenseMeta #-}

