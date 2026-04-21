{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeOperators         #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}

module Data.Array.Accelerate.Tabular.Prelude (
  the, unit
, map
) where

import Data.Array.Accelerate hiding (Scalar, the, unit, map, fold, foldAll, (!))
import Data.Array.Accelerate qualified as A
import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Data.Maybe

import Data.Array.Accelerate.Tabular.Prelude.Index
import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Prelude.Table
import Lens.Micro

import Data.Array.Accelerate.Data.Lens ()

map :: (Rep rep key, Elt val, Elt val')
    => (Exp val -> Exp val')
    -> Acc (Table rep key val)
    -> Acc (Table rep key val')
map f Table_ { meta_, vals_ } = Table_ meta_ (A.map (fmap f) vals_)

-- | Construct a single-elemement table from a scalar value.
unit :: (Elt val) => Exp val -> Acc (Scalar val)
unit x = Table_ emptyMeta $ generate (I1 1) (const $ Just_ x)

-- | Extract the element from a single-element table.
the :: (Elt val) => Acc (Scalar val) -> Exp val
the = (! Z_) -- Assuming a Scalar table always contains exactly one value.
