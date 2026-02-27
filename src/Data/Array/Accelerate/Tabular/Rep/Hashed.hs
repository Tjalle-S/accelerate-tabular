{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeOperators         #-}

{-# LANGUAGE FlexibleInstances     #-}

module Data.Array.Accelerate.Tabular.Rep.Hashed (
  Hashed
) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.HashMap ( HashMap, Hashable, fromVector )

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Rep.Snoc
import Data.Array.Accelerate.Tabular.Util

-- | Stores keys in a hash map.
-- Can only be used for the outermost dimension.
--
data Hashed

instance (Hashable key) => Rep (Z :.: Hashed) (Z :.: key) where

  type MetaR (Z :.: Hashed) (Z :.: key) = HashMap key Int

  emptyMeta = Meta_ (fromVector emptyVector)
  