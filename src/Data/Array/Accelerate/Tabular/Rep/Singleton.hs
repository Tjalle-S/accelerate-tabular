{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE PatternSynonyms       #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Array.Accelerate.Tabular.Rep.Singleton (
  Singleton
) where

import Data.Array.Accelerate

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Rep.Snoc
import Data.Array.Accelerate.Tabular.Util

-- | Stores a single key on this level for each key in the parent levels.
--
data Singleton

-- Singleton instances.
-- --------------------

instance (Rep rep keys, Elt key) => Rep (rep :.: Singleton) (keys :.: key) where

  type MetaR (rep :.: Singleton) (keys :.: key) = (Meta rep keys, Vector key)

  emptyMeta = SingletonMeta emptyMeta emptyVector


-- Local utilities.
-- ----------------

pattern SingletonMeta :: (Arrays (Meta rep keys), Elt key)
              => Acc (Meta rep keys)
              -> Acc (Vector key)
              -> Acc (Meta (rep :.: Singleton) (keys :.: key))
pattern SingletonMeta { met, ks } = Meta_ (T2 met ks)
{-# COMPLETE SingletonMeta #-}