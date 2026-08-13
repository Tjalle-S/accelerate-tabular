{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitForAll #-}

module Data.Array.Accelerate.Tabular.Prelude.Cartesian (
  type (++)
, cartesianWith
) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Monoid

import Data.Array.Accelerate.Tabular.Classes.Fold
import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Prelude.Assocs
import Data.Array.Accelerate.Tabular.Prelude.Table

-- | The type resulting from appending two keys.
--
type family xs ++ ys where
  xs ++ Z         = xs
  xs ++ (ys :. y) = (xs ++ ys) :. y

-- Reference implementation of cartesian product.

-- | Applies a function to all combinations of values in the tables.
-- The corresponding keys are concatenated.
-- 
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
  let 
      xs' = assocs' xs
      ys' = assocs' ys
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

data KeyR key where
  KeyRZ    :: KeyR Z
  KeyRSnoc :: (Key keys, Elt key)
           => KeyR keys
           -> Exp key
           -> KeyR (keys :. key)


class (Elt key) => Key key where

  getKeyR :: Exp key -> KeyR key
  toKey :: KeyR key -> Exp key

  proveKey :: (Elt k) => Exp k -> KeyR key -> Dict' Elt (k ++ key)

instance Key Z where

  getKeyR _ = KeyRZ
  toKey   _ = Z_

  proveKey _ _ = Dict'

instance (Key key, Elt k) => Key (key :. k) where

  getKeyR (ks ::. k) = KeyRSnoc (getKeyR ks) k
  toKey (KeyRSnoc ks k) = toKey ks ::. k
  
  proveKey p (KeyRSnoc ks _) = case proveKey p ks of
    Dict' -> Dict'
