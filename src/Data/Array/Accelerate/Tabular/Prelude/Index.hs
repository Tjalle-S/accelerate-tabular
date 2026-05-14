{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}

module Data.Array.Accelerate.Tabular.Prelude.Index (
  index, (!?), indexMany
, unsafeIndex, (!), unsafeIndexMany
) where

import Data.Array.Accelerate hiding ((!), Scalar)
import Data.Array.Accelerate.Control.Monad
import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Data.Maybe

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Prelude.Table
import Data.Array.Accelerate.Tabular.Util
import Data.Typeable

-- | Accesses the value at the given key.
-- If the given key is not present, returns 'Nothing_'.
--
index :: (Rep rep key, Elt val)
      => Acc (Table rep key val)
      -> Exp key
      -> Exp (Maybe val)
index Table_ { meta_, vals_ } key =
  case getIndexMeta meta_ of
    NoDict -> let res = lookup key $ zip (enumKeys meta_) vals_
              in  join res
    Dict   -> let mi = toLinearIndex meta_ key
              in  (vals_ !!) =<< mi

-- | Infix variant of 'index'.
--
(!?) :: (Rep rep key, Elt val)
     => Acc (Table rep key val)
     -> Exp key
     -> Exp (Maybe val)
(!?) = index

-- | Like 'index', but accesses values at multiple keys at once.
--
-- 'indexMany' may be more efficient than using @'Data.Array.Accelerate.map' 'index'@.
--
indexMany :: (Rep rep key, Elt val)
          => Acc (Table rep key val)
          -> Acc (Vector key)
          -> Acc (Vector (Maybe val))
indexMany Table_ { meta_, vals_ } keys =
  case getIndexMeta meta_ of
    NoDict -> map join $ lookupMany keys $ zip (enumKeys meta_) vals_
    Dict   -> let mis  = toLinearIndices meta_ keys
                  mmvs = map (fmap (vals_ !!)) mis
              in  map join mmvs


-- | Like 'index', but performs no checks.
-- If the key is not present, the behaviour of this function is undefined.
--
-- Only use if you are absolutely certain the key is present.
--
unsafeIndex :: (Rep rep key, Elt val)
            => Acc (Table rep key val)
            -> Exp key
            -> Exp val
unsafeIndex Table_ { meta_, vals_ } key =
  case getIndexMeta meta_ of
  NoDict -> let res = lookup key $ zip (enumKeys meta_) vals_
            in  fromJust (fromJust res)
  Dict   -> fromJust (vals_ !! unsafeToLinearIndex meta_ key)

-- | Infix variation of 'unsafeIndex'.
-- If the key is not present, the behaviour of this function is undefined.
--
-- Only use if you are absolutely certain the key is present.
--
(!) :: (Index rep key, Elt val)
    => Acc (Table rep key val)
    -> Exp key
    -> Exp val
(!) = unsafeIndex

-- | Like 'unsafeIndex', but accesses values at multiple keys at once.
--
-- 'unsafeIndexMany' may be more efficient than using @'Data.Array.Accelerate.map' 'unsafeIndex'@.
--
unsafeIndexMany :: (Rep rep key, Elt val)
                => Acc (Table rep key val)
                -> Acc (Vector key)
                -> Acc (Vector val)
unsafeIndexMany Table_ { meta_, vals_ } keys =
  case getIndexMeta meta_ of
    NoDict -> map (fromJust . fromJust)
            $ lookupMany keys
            $ zip (enumKeys meta_) vals_
    Dict   -> map (fromJust . (vals_ !!)) (unsafeToLinearIndices meta_ keys)

getIndexMeta :: forall rep key . (Rep rep key)
              => Acc (Meta rep key)
              -> MaybeDict (FastIndex rep) (Index rep key)
getIndexMeta _ = getIndexConstraint @rep @key Proxy Proxy
