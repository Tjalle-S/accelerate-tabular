{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE NamedFieldPuns        #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}

module Data.Array.Accelerate.Tabular.Rep.Singleton (
  Singleton, UnsafeCompleteSingleton
) where

import Data.Array.Accelerate

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Rep.Snoc
import Data.Array.Accelerate.Tabular.Util
import Data.Array.Accelerate.Unsafe (undef)

-- | Stores a single key on this level for each key in the parent levels.
--
data Singleton

-- | Stores a single key on this level for each key in the parent levels.
--
data UnsafeCompleteSingleton

-- Singleton instances.
-- --------------------

instance (Rep rep keys, Elt key) => Rep (rep :.: Singleton) (keys :.: key) where

  type MetaR (rep :.: Singleton) (keys :.: key) = (Meta rep keys, Vector key)

  emptyMeta = SingletonMeta emptyMeta emptyVector

instance (Rep rep keys, Elt key) =>
  Rep (rep :.: UnsafeCompleteSingleton) (keys :.: key) where

    type MetaR (rep :.: UnsafeCompleteSingleton) (keys :.: key) =
      (Meta rep keys, Vector key)

    emptyMeta = Meta_ $ T2 emptyMeta emptyVector

    createMeta ks =
      let
        (ks', is) = splitKeys ks
        (met, perm, n) = createMeta ks'

        perm' = map unindex1 perm
        target = fill (I1 n) undef

        met' = Meta_ $ T2 met (scatter perm' target is)
      in (met', perm, n)




-- Local utilities.
-- ----------------

pattern SingletonMeta :: (Arrays (Meta rep keys), Elt key)
              => Acc (Meta rep keys)
              -> Acc (Vector key)
              -> Acc (Meta (rep :.: Singleton) (keys :.: key))
pattern SingletonMeta { met, ks } = Meta_ (T2 met ks)
{-# COMPLETE SingletonMeta #-}
