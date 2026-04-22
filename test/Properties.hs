{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Properties (
  createPreservesAssocs
) where

import qualified Prelude as P
import Data.List (sort)

import Data.Proxy

import Data.Array.Accelerate hiding (assert)
import Data.Array.Accelerate.Tabular hiding (assert)

import Hedgehog

import Gen
import Data.Array.Accelerate.Tabular.Prelude.Table (NotScalarConstruct)


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

isPermutationOf :: (P.Ord a) => [a] -> [a] -> Bool
isPermutationOf xs ys = sort xs P.== sort ys
