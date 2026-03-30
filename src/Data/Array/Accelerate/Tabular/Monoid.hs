{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Array.Accelerate.Tabular.Monoid (
  module A
, Monoid
) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Monoid as A hiding ( Monoid )
import qualified Data.Array.Accelerate.Data.Monoid as A

type Monoid a = (Elt a, A.Monoid (Exp a))
