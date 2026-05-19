{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Array.Accelerate.Tabular.Prelude.Filter (
  filter
) where

import Data.Array.Accelerate hiding (Scalar, the, unit, indexed, (!), imap, filter)
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.Data.Maybe

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Prelude.Assocs  as Tabular.Prelude
import Data.Array.Accelerate.Tabular.Prelude.Table   as Tabular.Prelude
import Data.Data

-- | Return a table containing only the elements that fulfill the given condition.
--
filter :: (Rep rep' key, NotScalar key, Rep rep key, Elt val)
       => (Exp key -> Exp val -> Exp Bool)
       -> Acc (Table rep  key val)
       -> Acc (Table rep' key val)
filter p tab =
  let kvs   = massocs tab
  in  createTable' (isOrdered tab)
        $ afst
        $ compact
          (A.map (maybe False_ $ uncurry p) kvs)
          (A.map fromJust                   kvs)

isOrdered :: forall rep key val . (Rep rep key)
          => Acc (Table rep key val)
          -> AssumeOrd
isOrdered _ = case isOrderedProxy @rep Proxy of
  Nothing   -> NoAssumeOrdered
  Just Refl -> AssumeOrdered