{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Array.Accelerate.Tabular.Prelude.Filter (
  filter
) where

import Data.Array.Accelerate hiding ( filter )
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.Data.Maybe

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Prelude.Assocs
import Data.Array.Accelerate.Tabular.Prelude.Table

-- | Return a table containing only the elements that fulfill the given condition.
--
filter :: (Rep rep' key, NotScalar key, Rep rep key, Elt val)
       => (Exp key -> Exp val -> Exp Bool)
       -> Acc (Table rep  key val)
       -> Acc (Table rep' key val)
filter p tab =
  let kvs   = massocs tab
  in  createTable' (assumeOrdered tab)
        $ afst
        $ compact
          (A.map (maybe False_ $ uncurry p) kvs)
          (A.map fromJust                   kvs)
