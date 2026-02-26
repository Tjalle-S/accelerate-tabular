{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TemplateHaskell       #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module Data.Array.Accelerate.Tabular.Prelude (
  Table (..)
, pattern Table_, meta_, vals_
, project
, index, unsafeIndex
, unsafeUpdate
, emptyTable
) where

import Data.Array.Accelerate
import qualified Data.Array.Accelerate.Unsafe as Unsafe
import Data.Array.Accelerate.Data.Functor

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Classes.Index

data Table rep key val = Table {
  meta :: Meta rep key
, vals :: Vector val
} deriving (Generic)

deriving instance (Show (Meta rep key), Elt val, Show val) =>
  Show (Table rep key val)

instance (Arrays (Meta rep key), Elt val) => Arrays (Table rep key val)

{-# COMPLETE Table_ #-}
pattern Table_ :: (Arrays (Meta rep key), Elt val)
               => Acc (Meta rep key)
               -> Acc (Vector val)
               -> Acc (Table rep key val)
pattern Table_ { meta_, vals_ } = Pattern (meta_, vals_)

project :: (Rep rep key, Elt val, Elt val')
        => (Exp val -> Exp val')
        -> Acc (Table rep key val)
        -> Acc (Table rep key val')
project f Table_ { meta_, vals_ } = Table_ meta_ (map f vals_)

index :: (Index rep key, Elt val)
      => Acc (Table rep key val)
      -> Exp key
      -> Exp (Maybe val)
index Table_ { meta_, vals_ } k = fmap (vals_ !!) (toLinearIndex meta_ k)

unsafeIndex :: (Index rep key, Elt val)
            => Acc (Table rep key val)
            -> Exp key
            -> Exp val
unsafeIndex Table_ { meta_, vals_ } k = vals_ !! unsafeToLinearIndex meta_ k

unsafeUpdate :: (Index rep key, Elt val)
             => (Exp key -> Exp val -> Exp val)
             -> Acc (Vector key)
             -> Acc (Table rep key val)
             -> Acc (Table rep key val)
unsafeUpdate f ks Table_ { meta_, vals_ } = 
  let idxs = map (unsafeToLinearIndex meta_) ks
      ks'  = scatter idxs (fill (shape vals_) Nothing_) (map Just_ ks)
      vs   = zipWith f' vals_ ks'
  in  Table_ { meta_ = meta_, vals_ = vs }
  where
    f' v mk = match mk & \case
      Nothing_ -> v
      Just_ k  -> f k v

emptyTable :: (Rep rep key, Elt val) => Acc (Table rep key val)
emptyTable = Table_ {
  meta_ = emptyMeta
, vals_ = fill (I1 0) Unsafe.undef
}

-- createTable :: (Insert rep key, Elt val)
--             => Acc (Vector (key, val))
--             -> Acc (Table rep key val)
-- createTable kvs = 
--   let (ks, vs)     = unzip kvs
--       T2 meta perm = create ks
--       vals         = gather perm vs
  -- in  Table_ { meta_ = meta, vals_ = vals }
