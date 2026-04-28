{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE NamedFieldPuns    #-}

module Data.Array.Accelerate.Tabular.Prelude.Map (
  map
, imap
) where

import Data.Array.Accelerate hiding (map, imap)
import qualified Data.Array.Accelerate as A

import Data.Array.Accelerate.Data.Functor

import Data.Array.Accelerate.Tabular.Prelude.Table
import Data.Array.Accelerate.Tabular.Classes.Rep

-- | Apply the given function element-wise to a table.
--
map :: (Rep rep key, Elt val, Elt val')
    => (Exp val -> Exp val')
    -> Acc (Table rep key val)
    -> Acc (Table rep key val')
map f Table_ { meta_, vals_ } = Table_ meta_ $ A.map (fmap f) vals_

-- | Apply the given function to each element and its key in a table.
--
imap :: (Rep rep key, Elt val, Elt val')
     => (Exp key -> Exp val -> Exp val')
     -> Acc (Table rep key val)
     -> Acc (Table rep key val')
imap f (Table_ { meta_, vals_}) = 
  let kvs = zipWith (\k v -> T2 k <$> v) (enumKeys meta_) vals_
  in  Table_  meta_ $ A.map (fmap $ uncurry f) kvs
