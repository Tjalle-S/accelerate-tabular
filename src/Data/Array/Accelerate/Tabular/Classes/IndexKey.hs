{-# LANGUAGE NoImplicitPrelude    #-}

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Array.Accelerate.Tabular.Classes.IndexKey (
  IndexKey (..)
) where

import Data.Array.Accelerate

-- | Keys that can be used as a linear index.
--
class (Ord a) => IndexKey a where

  -- | Convert a linear index to a key.
  --
  toKey :: Exp Int -> Exp a

  -- | Convert a key to a linear index.
  --
  fromKey :: Exp a -> Exp Int


-- Any type that can be converted from and to an Int can be used as key.
-- It may be cleaner to manually write these instead, for tighter control.
instance {-# OVERLAPPABLE #-}
  (Integral a, FromIntegral Int a, FromIntegral a Int) => IndexKey a where

  toKey   = fromIntegral
  fromKey = fromIntegral

instance IndexKey Char where
  toKey   = chr
  fromKey = ord
