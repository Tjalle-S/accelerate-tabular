{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Data.Array.Accelerate.Tabular.Prelude.Index (
  index, (!?)
, unsafeIndex, (!)
) where

import Data.Array.Accelerate hiding ((!), Scalar)
import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Data.Maybe

import Data.Array.Accelerate.Tabular.Classes.Index
import Data.Array.Accelerate.Tabular.Prelude.Table

-- | Accesses the value at the given key.
-- If the given key is not present, returns 'Nothing_'.
--
index :: (Index rep key, Elt val)
      => Acc (Table rep key val)
      -> Exp key
      -> Exp (Maybe val)
index Table_ { meta_, vals_ } key = 
  let mi = toLinearIndex meta_ key
      mmv = fmap (vals_ !!) mi
  in  mmv & match \case
        Nothing_ -> Nothing_
        Just_ mv -> mv
      
-- | Infix variant of 'index'.
--
(!?) :: (Index rep key, Elt val)
     => Acc (Table rep key val)
     -> Exp key
     -> Exp (Maybe val)
(!?) = index

-- | Like 'index', but performs no checks.
-- If the key is not present, the behaviour of this function is undefined.
--
-- Only use if you are absolutely certain the key is present.
--
unsafeIndex :: (Index rep key, Elt val)
            => Acc (Table rep key val)
            -> Exp key
            -> Exp val
unsafeIndex Table_ { meta_, vals_ } key = 
  fromJust (vals_ !! unsafeToLinearIndex meta_ key)

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
