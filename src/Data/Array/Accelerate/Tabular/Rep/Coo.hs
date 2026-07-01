{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Data.Array.Accelerate.Tabular.Rep.Coo (Coo) where

import Data.Array.Accelerate

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Util

-- | Specialised representation for the COO format (i.e. list of key-value pairs.)
--
data Coo

instance (Eq key) => Rep Coo key where

  type MetaR Coo key = Vector key

  emptyMeta = Meta_ emptyVector

  createMeta _ ks = T3
    (Meta_ ks)
    (generate (I1 $ length ks) id)
    (unit $ length ks)

  enumKeys (Meta_ ks) = ks
