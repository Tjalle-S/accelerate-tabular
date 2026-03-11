{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TemplateHaskell       #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data.Array.Accelerate.Tabular.Rep.Snoc (
  type (:.:) (..)
, type Z     (..)
, pattern (::.:)
, key, unKey
) where

import Prelude ( Show (..), showString )
import qualified Prelude as P

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Semigroup
import Control.DeepSeq (NFData)

-- | Increase an index or representation rank by one dimension.
-- The ':.:' operator is used to construct both values and types.
--
-- In an 'Acc' context, the pattern '(::.:)' can be used instead.
--
infixl 3 :.:
data tail :.: head = tail :.: head
  deriving (Generic, P.Eq, P.Ord, NFData)

instance (Elt tail, Elt head) => Elt (head :.: tail)

instance (Show tail, Show head) => Show (tail :.: head) where
  showsPrec p (t :.: h) =
    showsPrec p t . showString " :.: " . showsPrec p h

mkPattern ''(:.:)

instance (Eq tail, Eq head) => Eq (tail :.: head) where
  (t1 ::.: h1) == (t2 ::.: h2) = (t1 == t2) && (h1 == h2)

instance (Ord tail, Ord head) => Ord (tail :.: head) where
  compare (t1 ::.: h1) (t2 ::.: h2) = compare t1 t2 <> compare h1 h2

key :: (Elt keys, Elt key) => Exp keys -> Exp key -> Exp (keys :.: key)
key = (::.:)

unKey :: (Elt keys, Elt key) => Exp (keys :.: key) -> Exp (keys, key)
unKey (ks ::.: k) = T2 ks k
