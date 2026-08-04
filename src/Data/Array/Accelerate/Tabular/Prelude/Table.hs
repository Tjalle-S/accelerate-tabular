{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Array.Accelerate.Tabular.Prelude.Table (
  Table (..)
, pattern Table_, meta_, vals_
, Scalar
, Vector
, fromArray, toArray

, emptyTable, createTable', createTable, orderedCreateTable
, assumeOrdered

, scalarFromAcc, scalarToAcc
) where

import qualified Prelude as P
import Data.Maybe (catMaybes)
import GHC.IsList

import Data.Array.Accelerate hiding (Scalar, Vector, unit, the, fromList, toList)
import qualified Data.Array.Accelerate as A

import Data.Array.Accelerate.Tabular.Classes.Rep

import Control.DeepSeq (NFData)
import Data.Array.Accelerate.Tabular.Util (emptyVector)
import Data.Type.Equality
import Data.Proxy
import Data.Array.Accelerate.Tabular.Rep
import Data.Array.Accelerate.Data.Maybe
import qualified Data.Maybe as P

-- | A table, consisting of metadata and values.
--
data Table rep key val = Table {
  -- | The metadata storing the present keys.
  --
  meta :: Meta rep key
  -- | The array of values.
  --
, vals :: A.Vector (Maybe val)
} deriving (Generic)

deriving instance (Show (Meta rep key), Elt val, Show val) =>
  Show (Table rep key val)
instance (Arrays (Meta rep key), Elt val) => Arrays (Table rep key val)
instance (NFData (Meta rep key), Elt val) => NFData (Table rep key val)

pattern Table_ :: (Arrays (Meta rep key), Elt val)
               => Acc (Meta rep key)
               -> Acc (A.Vector (Maybe val))
               -> Acc (Table rep key val)
pattern Table_ { meta_, vals_ } = Pattern (meta_, vals_)
{-# COMPLETE Table_ #-}

-- | Scalar tables hold a single value.
--
type Scalar = Table Z Z

type Vector = Table (Z :. Dense) (Z :. Int)

instance (Elt a) => IsList (Vector a) where

  type Item (Vector a) = a

  fromList xs = fromListN (P.length xs) xs
  toList = catMaybes . A.toList . vals

  fromListN n xs = Table {
    meta = Meta (Meta (), A.fromList Z [n])
  , vals = fromList (P.map Just xs)
  }

instance (Eq key, Elt val) => IsList (Table Coo key val) where

  type Item (Table Coo key val) = (key, val)

  fromList xs = fromListN (P.length xs) xs
  toList Table { meta = Meta keys, vals } = P.zip (toList keys) (P.map P.fromJust $ toList vals)

  fromListN n xs = Table {
    meta = Meta (fromListN n $ P.map P.fst xs)
  , vals = fromListN n $ P.map (Just . P.snd) xs
  }
  

-- | Convert an Accelerate array to a 'Vector'.
fromArray :: (Elt a) => Acc (A.Vector a) -> Acc (Vector a)
fromArray xs = Table_
  (Meta_ $ T2 emptyMeta (A.unit $ length xs))
  (A.map Just_ xs)

-- | Convert a 'Vector' to an Accelerate array.
--
toArray :: (Elt a) => Acc (Vector a) -> Acc (A.Vector a)
toArray = afst . justs . vals_

-- | Create an empty table.
-- 
emptyTable :: (Rep rep key, Elt val)
           => Acc (Table rep key val)
emptyTable = Table_ {
  meta_ = emptyMeta
, vals_ = emptyVector
}

createTable' :: (Rep rep key, Elt val)
             => AssumeOrd
             -> Acc (A.Vector (key, val))
             -> Acc (Table rep key val)
createTable' o kvs = 
  let (ks, vs)      = unzip kvs
      T3 met perm n = createMeta o ks

      perm'  = map unindex1 perm
      target = fill (I1 $ A.the n) Nothing_
      vs'    = map Just_ vs

  in  Table_ met (scatter perm' target vs')

assumeOrdered :: forall rep key val . (Rep rep key)
              => Acc (Table rep key val)
              -> AssumeOrd
assumeOrdered _ = case isOrderedProxy @rep Proxy of
  Nothing   -> NoAssumeOrdered
  Just Refl -> AssumeOrdered

-- | Construct a new table from the given keys and values.
--
createTable :: (Rep rep key, Elt val)
            => Acc (Vector (key, val))
            -> Acc (Table rep key val)
createTable = createTable' NoAssumeOrdered . toArray

-- | Variant of 'createTable' that assumes the input is already ordered by key.
-- 
-- It will be more efficient for most representations that require ordering, 
-- but do not support O(1) indexing.
--
orderedCreateTable :: (Rep rep key, Elt val)
                   => Acc (Vector (key, val))
                   -> Acc (Table rep key val)
orderedCreateTable = createTable' AssumeOrdered . toArray

scalarFromAcc :: Elt a => Acc (A.Scalar a) -> Acc (Scalar a)
scalarFromAcc x = Table_ (Meta_ (lift ())) (reshape (Z_ ::. 1) (A.map Just_ x))

scalarToAcc :: Elt a => Acc (Scalar a) -> Acc (A.Scalar a)
scalarToAcc = A.map fromJust . reshape Z_ . vals_
