{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE NamedFieldPuns        #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Array.Accelerate.Tabular.Rep.Singleton (
  UnsafeCompleteSingleton
) where

import Data.Array.Accelerate

import Data.Array.Accelerate.Tabular.Classes.Rep

import Data.Array.Accelerate.Tabular.Util
import Data.Array.Accelerate.Unsafe (undef)
import Data.Array.Accelerate.Tabular.Classes.Fold

import Prelude (type (~))


-- | Stores a single key on this level for each key in the parent levels.
--
data Singleton

-- | Stores a single key on this level for each key in the parent levels.
-- Assumes that there is exactly 1 child key for each parent.
--
-- Note that no additional checks are performed. In particular:
--
-- - If a single parent has multiple child keys, only 1 is stored, non-deterministically.
-- - If a parent key has no child keys, the key stored is undefined.
--
data UnsafeCompleteSingleton

-- Singleton instances.
-- --------------------

instance (Rep rep keys, Elt key) => Rep (rep :. Singleton) (keys :. key) where

  type MetaR (rep :. Singleton) (keys :. key) = (Meta rep keys, Vector key)

  emptyMeta = SingletonMeta emptyMeta emptyVector


-- Unsafe Singleton instances.
-- ---------------------------

instance (Rep rep keys, Elt key) =>
  Rep (rep :. UnsafeCompleteSingleton) (keys :. key) where

    type MetaR (rep :. UnsafeCompleteSingleton) (keys :. key) =
      (Meta rep keys, Vector key)

    emptyMeta = Meta_ $ T2 emptyMeta emptyVector

    createMeta ks =
      let
        (ks', is)     = splitKeys ks
        T3 met perm n = createMeta ks'

        perm' = map unindex1 perm
        target = fill (I1 $ the n) undef

        met' = Meta_ $ T2 met (scatter perm' target is)
      in T3 met' perm n


instance (Fold rep keys, Elt key) =>
  Fold (rep :. UnsafeCompleteSingleton) (keys :. key) where

  foldMeta d dmet@SingletonMeta { met } =
    case d of
      FoldKeep       -> T2 dmet (asnd $ foldMeta FoldKeep met)
      FoldGroup rest -> foldMeta rest met
  


-- Local utilities.
-- ----------------

type SingletonMetaR rep keys key = (Meta rep keys, Vector key)

type IsSingleton rep r keys key =
  ( Rep   (rep :. r) (keys :. key)
  , MetaR (rep :. r) (keys :. key) ~ SingletonMetaR rep keys key
  )

pattern SingletonMeta :: ( IsSingleton rep r keys key
                         , Arrays (Meta rep keys)
                         , Elt key
                         )
                      => Acc (Meta rep keys)
                      -> Acc (Vector key)
                      -> Acc (Meta (rep :. r) (keys :. key))
pattern SingletonMeta { met, ks } = Meta_ (T2 met ks)
{-# COMPLETE SingletonMeta #-}
