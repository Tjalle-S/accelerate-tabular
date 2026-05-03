{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Properties (
  createPreservesAssocs
, foldAllSameAsFoldr
, filterPreservesFiltered
) where

import qualified Prelude as P
import Data.List (sort)

import Data.Proxy

import Data.Array.Accelerate hiding (assert, foldAll, filter, the)
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.Tabular hiding (assert)

import Hedgehog

import Gen
import Data.Array.Accelerate.Tabular.Prelude.Table (NotScalarConstruct)
import Data.Array.Accelerate.Tabular.Prelude.Fold (NotScalarFold)


createPreservesAssocs :: forall rep key val
                      .  ( NotScalarConstruct rep
                         , Show key, Show val
                         , P.Ord key, P.Ord val
                         , Ord val
                         , Rep rep key
                         )
                      => RunN
                      -> Proxy rep
                      -> (DIM1 -> Gen (Vector (key, val)))
                      -> Property
createPreservesAssocs runN _ gen = property $ do
  len <- forAll dim1
  kvs <- forAll (gen len)

  let !go = runN $ assocs . createTable @rep @key @val

  let kvs' = toList (go kvs)
  annotateShow kvs'

  assert $  kvs' `isPermutationOf` toList kvs

filterPreservesFiltered :: forall rep key val
                        .  ( NotScalarConstruct rep
                          , Show key, Show val
                          , P.Ord key, P.Ord val, P.Integral val
                          , Ord val, Integral val
                          , Rep rep key
                          )
                        => RunN
                        -> Proxy rep
                        -> (DIM1 -> Gen (Vector (key, val)))
                        -> Property
filterPreservesFiltered runN _ gen = property $ do
  len <- forAll dim1
  kvs <- forAll (gen len)

  let !go = runN $ assocs . filter @rep (const even) . createTable @rep @key @val

  let kvs' = toList (go kvs)
  annotateShow kvs'

  assert $  kvs' `isPermutationOf` P.filter (P.even . P.snd) (toList kvs)

isPermutationOf :: (P.Ord a) => [a] -> [a] -> Bool
isPermutationOf xs ys = sort xs P.== sort ys


foldAllSameAsFoldr :: forall rep key val
                   . ( NotScalarConstruct rep
                     , NotScalarFold rep
                     , Show key, Show val
                     , P.Eq val, P.Num val, Num val
                     , Rep rep key
                     )
                   => RunN
                   -> Proxy rep
                   -> (DIM1 -> Gen (Vector (key, val)))
                   -> Property
foldAllSameAsFoldr runN _ gen = property $ do
  len <- forAll dim1
  kvs <- forAll (gen len)

  let !go = runN $ A.unit . the . foldAll (+) 0 . createTable @rep @key @val

  let res = indexArray (go kvs) Z
  annotateShow res

  res === P.foldr ((+) . P.snd) 0 (toList kvs)
