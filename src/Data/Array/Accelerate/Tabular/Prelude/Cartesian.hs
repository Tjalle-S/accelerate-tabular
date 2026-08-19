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

module Data.Array.Accelerate.Tabular.Prelude.Cartesian (
  type (++)
, cartesianWith
, concatKey
, Key (..)
, KeyR (..)
, SomeKeyR (..)
) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Monoid

import Data.Array.Accelerate.Tabular.Classes.Fold
import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Prelude.Assocs
import Data.Array.Accelerate.Tabular.Prelude.Table

import Data.Type.Equality
import Data.Data
-- import Data.Array.Accelerate.Tabular.Classes.Sugar

type family xs ++ ys where
  xs ++ Z         = xs
  xs ++ (ys :. y) = (xs ++ ys) :. y

cartesianWith :: forall rep'' rep' rep key'' key' key c b a
              . ( Key key'
                , Rep rep key, Rep rep' key', Rep rep'' key''
                , Elt a, Elt b, Elt c
                , key'' ~ (key ++ key')
                )
              => (Exp a -> Exp b -> Exp c)
              -> Acc (Table rep key a)
              -> Acc (Table rep' key' b)
              -> Acc (Table rep'' key'' c)
cartesianWith f xs ys =
  let xs' = assocs xs
      ys' = assocs ys
      xs'' = replicate (Z_ ::. All_       ::. length ys') xs'
      ys'' = replicate (Z_ ::. length xs' ::. All_)       ys'
  in  createTable' (assumeOrdered xs <> assumeOrdered ys)
        $ flatten
        $ zipWith combine xs'' ys''
  where
    combine (T2 xk xv) (T2 yk yv) =
      let k' = concatKey xk (getKeyR yk)
      in  T2 k' (f xv yv)

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


-- concatKey' :: KeyR key -> KeyR key' -> KeyR (key ++ key')
-- concatKey' key KeyRZ           = key
-- concatKey' key (KeyRSnoc ks k) = KeyRSnoc (concatKey' key ks) k

-- Only require this for right key (can append to anything).

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

  -- proveKey' p (KeyRSnoc ks _) = case proveKey' p ks of
  --   Dict' -> Dict'

-- proveKey :: forall keys key . (Key key, Key keys) => Exp keys -> KeyR key -> Dict' Key (keys ++ key)
-- proveKey _ _ = proveKey' @key @keys Proxy Proxy
