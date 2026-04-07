{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TemplateHaskell       #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}

module Data.Array.Accelerate.Tabular.Rep.Snoc (
  type Z     (..), pattern Z_
, type (:.:) (..), pattern (::.:)
, key, unKey
) where

import Prelude ( Show (..), showString )
import qualified Prelude as P

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Semigroup

import Lens.Micro

-- | Increase an index or representation rank by one dimension.
-- The ':.:' operator is used to construct both values and types.
--
-- In an 'Exp' context, the pattern '(::.:)' can be used instead.
--
infixl 3 :.:
data tail :.: head = !tail :.: !head
  deriving (Generic, P.Eq, P.Ord, Elt)

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

keyHead :: (Elt keys, Elt key) => Exp (keys :.: key) -> Exp key
keyHead (_ ::.: k) = k

keyTail :: (Elt keys, Elt key) => Exp (keys :.: key) -> Exp keys
keyTail (ks ::.: _) = ks

-- Accessing specific elements of a key.
-- -------------------------------------

instance (Elt keys, Elt key, Elt key') =>
  Field1 (Exp (keys :.: key)) (Exp (keys :.: key')) (Exp key) (Exp key') where
  _1 = lens keyHead (\ks -> (keyTail ks ::.:))

instance (Elt ks, Elt a, Elt b, Elt b') =>
  Field2 (Exp (ks :.: b :.: a)) (Exp (ks :.: b' :.: a)) (Exp b) (Exp b') where
  _2 = lens (\(_ ::.: b ::.: _)     -> b)
            (\(ks ::.: _ ::.: a) b' -> ks ::.: b' ::.: a)

instance (Elt ks, Elt a, Elt b, Elt c, Elt c') =>
  Field3 (Exp (ks :.: c :.: b :.: a)) (Exp (ks :.: c' :.: b :.: a))
    (Exp c) (Exp c') where
  _3 = lens (\(_  ::.: c ::.: _ ::.: _)    -> c)
            (\(ks ::.: _ ::.: b ::.: a) c' -> ks ::.: c' ::.: b ::.: a)
