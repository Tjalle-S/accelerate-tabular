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

module Data.Array.Accelerate.Tabular.Prelude (
  emptyTable
, createTable
, index, unsafeIndex
, (!?), (!)
, map
, Scalar, unit, the
, foldAll
, fold1All
) where

import Data.Array.Accelerate hiding (Scalar, the, unit, map, fold, foldAll, (!))
import Data.Array.Accelerate qualified as A
import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Data.Maybe

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Classes.Index
import Data.Array.Accelerate.Tabular.Table
import Data.Array.Accelerate.Tabular.Util

-- | Scalar tables hold a single value.
type Scalar = Table Z Z

-- | Construct a single-elemement table from a scalar value.
unit :: (Elt val) => Exp val -> Acc (Scalar val)
unit x = Table_ emptyMeta $ generate (I1 1) (const $ Just_ x)

-- | Extract the element from a single-element table.
the :: (Elt val) => Acc (Scalar val) -> Exp val
the = (! Z_) -- Assuming a Scalar table always contains exactly one value.

map :: (Rep rep key, Elt val, Elt val')
    => (Exp val -> Exp val')
    -> Acc (Table rep key val)
    -> Acc (Table rep key val')
map f Table_ { meta_, vals_ } = Table_ meta_ (A.map (fmap f) vals_)


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
--
(!) :: (Index rep key, Elt val)
    => Acc (Table rep key val)
    -> Exp key
    -> Exp val
(!) = unsafeIndex


-- | Reduction of a table of arbitrary dimensionality to a single scalar value.
-- The first argument needs to be function that is both associative /and/ commutative.
--
foldAll :: (Rep rep key, Elt val)
        => (Exp val -> Exp val -> Exp val)
        -> Exp val
        -> Acc (Table rep key val)
        -> Acc (Scalar val)
foldAll f e Table_ { vals_ } = 
  let res = A.fold (combineMaybe f) (Just_ e) vals_
  in  Table_ emptyMeta (flatten res)

