{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}

module Data.Array.Accelerate.Tabular.Classes.Index (
  Index (..)
) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Maybe

import Data.Array.Accelerate.Tabular.Classes.Rep

-- | Representations that support efficient indexing.
--
class (Rep rep key) => Index rep key where
  
  -- | Convert a key into an index into the value array.
  -- No additional checks are performed.
  unsafeToLinearIndex :: Acc (Meta rep key)
                      -> Exp key
                      -> Exp Int
  unsafeToLinearIndex met = fromJust . toLinearIndex met

  -- | Convert a key into an index into the value array.
  -- If the key is not present, returns 'Nothing'.
  toLinearIndex :: Acc (Meta rep key)
                -> Exp key
                -> Exp (Maybe Int)
  toLinearIndex met = Just_ . unsafeToLinearIndex met

  {-# MINIMAL unsafeToLinearIndex | toLinearIndex #-}

instance Index Z Z where
  unsafeToLinearIndex _ _ = 0
  toLinearIndex       _ _ = Just_ 0
