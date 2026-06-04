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

import Data.Array.Accelerate.Tabular.Classes.IndexKey
import Data.Array.Accelerate.Tabular.Util
import Data.Array.Accelerate.Tabular.Classes.Fold
import Data.Typeable

-- | All keys on this level are present, and the associated values are 
-- explicitly stored.
--
-- A representation consisting of only 'Dense' levels is equivalent to an array.
--
data Dense


-- Dense instances.
-- ----------------

instance (Rep rep keys, IndexKey key) =>
  Rep (rep :. Dense) (keys :. key) where

  type MetaR (rep :. Dense) (keys :. key) = (Meta rep keys, Scalar Int)

  type Ordered   (rep :. Dense) = Ordered   rep
  type FastIndex (rep :. Dense) = FastIndex rep

  getIndexConstraint _ _ =
    case getIndexConstraint (Proxy @rep) (Proxy @keys) of
      NoDict -> NoDict
      Dict   -> Dict
    

  emptyMeta = DenseMeta emptyMeta (unit $ toKey 0)

  createMeta o ks =
    let (ks', is)     = splitKeys ks
        T3 met perm n = createMeta o ks'

        n'   = 1 + fromKey (the $ fold max (toKey 0) is)
        met' = DenseMeta met (unit n')

        perm' = zipWith (\(I1 p) i -> I1 $ toKey $ p * n' + fromKey i) perm is
    in  T3 met' perm' (zipWith (*) n (unit n'))

  enumKeys DenseMeta { met, n } = expand
    (const $ the n)
    (\k i -> k ::. toKey i)
    (enumKeys met)


instance (Index rep keys, IndexKey key) => Index (rep :. Dense) (keys :. key) where

  unsafeToLinearIndex DenseMeta { met, n } (k ::. i) =
    fromKey (the n) * unsafeToLinearIndex met k + fromKey i

  toLinearIndex       DenseMeta { met, n } (k ::. i) =
    if i >= toKey 0 && fromKey i < the n
      then fmap (\i' -> fromKey (the n) * i' + fromKey i) (toLinearIndex met k)
      else Nothing_

  unsafeToLinearIndices DenseMeta { met, n } ks = 
    let (ks', is) = splitKeys ks
    in  zipWithChecked (\i k -> fromKey (the n) * i + fromKey k)
        (unsafeToLinearIndices met ks')
        is
    
  toLinearIndices DenseMeta { met, n } ks =
    let (ks', is) = splitKeys ks
    in  zipWithChecked f (toLinearIndices met ks') is
    where
      f i k = if k >= toKey 0 && fromKey k < the n
                 then fmap (\i' -> fromKey (the n) * i' + fromKey k) i
                 else Nothing_


instance (Fold rep keys, IndexKey key) =>
  Fold (rep :. Dense) (keys :. key) where

  foldMeta d dmet@DenseMeta { met, n } =
    case d of
      FKeep ->
        let (T2 _ seg, _) = foldMeta FKeep met
            len           = sum seg
            seg'          = fill (I1 $ the len) (the n)
        in  (T2 dmet seg', Dict')
      FGroup FKeep ->
        let (T2 met' seg, _) = foldMeta FKeep met
            len              = sum seg
            seg'             = fill (I1 $ the len) (the n)
        in  (T2 met' seg', Dict')
      FGroup rest -> 
        let (res, prf) = foldMeta rest met
        in  withDict' prf $
          let T2 met' seg = withDict' prf res
              seg'        = map (* the n) seg
          in  (T2 met' seg', Dict')


-- Local utilities.
-- ----------------

pattern DenseMeta :: (Arrays (Meta rep keys), Elt key)
              => Acc (Meta rep keys)
              -> Acc (Scalar Int)
              -> Acc (Meta (rep :. Dense) (keys :. key))
pattern DenseMeta { met, n } = Meta_ (T2 met n)
{-# COMPLETE DenseMeta #-}
