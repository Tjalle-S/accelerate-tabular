{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TemplateHaskell       #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}

{-# LANGUAGE DeriveGeneric         #-}

module Data.Array.Accelerate.Tabular.Rep.Snoc (
  type (:.:) (..)
, type Z     (..)
, pattern (::.:)
) where

import Prelude ( Show (..), showString )

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Semigroup

-- | Increase an index or representation rank by one dimension.
-- The ':.:' operator is used to construct both values and types.
--
infixl 3 :.:
data tail :.: head = tail :.: head
  deriving (Generic)

instance (Elt tail, Elt head) => Elt (head :.: tail)

instance (Show tail, Show head) => Show (tail :.: head) where
  showsPrec p (t :.: h) =
    showsPrec p t . showString " :.: " . showsPrec p h

mkPattern ''(:.:)
