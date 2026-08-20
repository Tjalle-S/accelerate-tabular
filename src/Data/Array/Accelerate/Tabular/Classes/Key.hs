{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Array.Accelerate.Tabular.Classes.Key (
  module Data.Array.Accelerate.Tabular.Classes.Key
) where

import Data.Array.Accelerate

import Data.Array.Accelerate.Tabular.Classes.Fold

import Data.Data

-- | The type resulting from appending two keys.
--
type family xs ++ ys where
  xs ++ Z         = xs
  xs ++ (ys :. y) = (xs ++ ys) :. y

concatKey :: (Elt key, key'' ~ key ++ key')
          => Exp key
          -> KeyR key'
          -> Exp key''
concatKey k k' =
  case k' of
    KeyRZ -> k
    KeyRSnoc kr k'' -> case proveKey k kr of
      Dict' -> concatKey k kr ::. k''

  --toKey $ concatKey' (getKeyR k) (getKeyR k')

data KeyR key where
  KeyRZ    :: KeyR Z
  KeyRSnoc :: (Key keys, Elt key)
           => KeyR keys
           -> Exp key
           -> KeyR (keys :. key)

class (Elt key) => Key key where

  getKeyR :: Exp key -> KeyR key
  toKey :: KeyR key -> Exp key

  proveKey  :: (Elt k) => Exp k -> KeyR key -> Dict' Elt (k ++ key)

  proveKey' :: Key k => Proxy k -> Proxy key -> Dict' Key (k ++ key)
  -- proveKey' :: (Key k) => Exp k -> KeyR key -> Dict' Key (k ++ key)

instance Key Z where

  getKeyR _ = KeyRZ
  toKey   _ = Z_

  proveKey _ _ = Dict'

  proveKey' _ _ = Dict'


instance (Key key, Elt k) => Key (key :. k) where

  getKeyR (ks ::. k) = KeyRSnoc (getKeyR ks) k
  toKey (KeyRSnoc ks k) = toKey ks ::. k
  
  proveKey p (KeyRSnoc ks _) = case proveKey p ks of
    Dict' -> Dict'

  proveKey' p _ = case proveKey' p (Proxy @key) of
    Dict' -> Dict'

data SomeKeyR where
  TheKeyR :: KeyR key -> SomeKeyR 
