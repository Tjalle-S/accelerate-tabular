{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}

module Properties (
  createPreservesAssocs
, foldAllSameAsFoldr
, filterPreservesFiltered
, indexingSameAsLookup
, innerJoinReference
) where

import qualified Prelude as P
import Data.List (sort)

import Data.Proxy

import Data.Array.Accelerate hiding (assert, foldAll, filter, the)
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.Tabular hiding (assert)
import Data.Array.Accelerate.Tabular.Prelude


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
                          , P.Ord key, P.Integral val, Integral val
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

indexingSameAsLookup :: forall rep key val
                     . ( NotScalarConstruct rep
                       , Show key, Show val, Elt val
                       , P.Eq key, P.Eq val
                       , Rep rep key
                       )
                     => RunN
                     -> Proxy rep
                     -> (DIM1 -> Gen (Vector (key, val)))
                     -> (DIM1 -> Gen (Vector key))
                     -> Property
indexingSameAsLookup runN _ genKVs genLookups = property $ do
  len <- forAll dim1
  kvs <- forAll (genKVs len)

  ks   <- forAll (genLookups len)

  -- Make sure some keys are searched that are actually present.
  let Z :. len' = len
      hlen = len' `P.div` 2
      ks' = P.take hlen (toList ks) P.++ P.take hlen (P.map P.fst $ toList kvs)
      ks'' = fromList (Z :. P.length ks') ks'

  let !go = runN $ \kvs' ks''' -> indexMany (createTable @rep kvs') ks'''

  let res = toList (go kvs ks'')
  annotateShow res

  res === P.map (`P.lookup` toList kvs) ks'

innerJoinReference :: forall rep rep' rep'' key val
                    . ( NotScalarConstruct rep
                      , NotScalarConstruct rep'
                      , NotScalarConstruct rep''
                      , Show key, Show val
                      , P.Ord key, P.Ord val, P.Num val, Num val
                      , Rep rep key
                      , Rep rep' key
                      , Rep rep'' key
                      )
                    => RunN
                    -> Proxy rep
                    -> Proxy rep'
                    -> Proxy rep''
                    -> (DIM1 -> Gen (Vector (key, val)))
                    -> Property
innerJoinReference runN _ _ _ gen = property $ do
  len1 <- forAll dim1
  len2 <- forAll dim1

  kvs1 <- forAll (gen len1)
  kvs2 <- forAll (gen len2)

  let !go = runN $ go'

  let res = toList (go kvs1 kvs2)
  annotateShow res

  let ref = [ (k1, v1 + v2)
            | (k1, v1) <- toList kvs1
            , (k2, v2) <- toList kvs2
            , k1 P.== k2
            ]

  assert $ res `isPermutationOf` ref
  where
    go' :: Acc (Vector (key, val))
        -> Acc (Vector (key, val))
        -> Acc (Vector (key, val))
    go' kvs1 kvs2 = assocs
                  $ innerjoin @rep'' (+)
                    (createTable @rep  kvs1)
                    (createTable @rep' kvs2)
