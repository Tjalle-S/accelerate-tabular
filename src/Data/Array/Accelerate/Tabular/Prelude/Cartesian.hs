{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExplicitForAll #-}

module Data.Array.Accelerate.Tabular.Prelude.Cartesian (
  type (++)
, cartesianWith
) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Monoid

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Prelude.Assocs
import Data.Array.Accelerate.Tabular.Prelude.Table

import Data.Type.Equality

type family xs ++ ys where
  xs ++ Z         = xs
  xs ++ (ys :. y) = (xs ++ ys) :. y

cartesianWith :: forall rep'' rep' rep key'' key' key c b a
              . ( Key key, Key key', Key key''
                , Rep rep key, Rep rep' key', Rep rep'' key''
                , Elt a, Elt b, Elt c
                , key'' ~ key ++ key'
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
      let k' = concatKey xk yk
      in  T2 k' (f xv yv)

concatKey :: (Key key, Key key', Key key'', key'' ~ key ++ key')
          => Exp key
          -> Exp key'
          -> Exp key''
concatKey k k' = toKey $ concatKey' (getKeyR k) (getKeyR k')

data KeyR key where
  KeyRZ :: KeyR Z
  KeyRSnoc :: KeyR keys
           -> Exp key
           -> KeyR (keys :. key)


concatKey' :: KeyR key -> KeyR key' -> KeyR (key ++ key')
concatKey' key KeyRZ           = key
concatKey' key (KeyRSnoc ks k) = KeyRSnoc (concatKey' key ks) k

class (Elt key) => Key key where

  getKeyR :: Exp key -> KeyR key
  toKey :: KeyR key -> Exp key

instance Key Z where

  getKeyR _ = KeyRZ
  toKey   _ = Z_

instance (Key key, Elt k) => Key (key :. k) where

  getKeyR (ks ::. k) = KeyRSnoc (getKeyR ks) k
  toKey (KeyRSnoc ks k) = toKey ks ::. k
  