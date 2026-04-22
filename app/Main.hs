{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Prelude

import Data.Array.Accelerate (zip, zipWith, foldSeg, zipWith3)
import Data.Array.Accelerate.LLVM.Native
import Data.Array.Accelerate.Tabular.Rep
import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular
import Data.Array.Accelerate.Tabular.Classes.Fold
import Data.Array.Accelerate.Tabular.Prelude.Table
import Data.Proxy (Proxy (Proxy))

type I2 = Z :. Int :. Int
type D2 = Z :. Dense :. Dense

type CD = Z :. OrdCompressed :. Dense

type I1 = Z :. Int
type Sparse = Z :. OrdCompressed

type H = Z :. Hashed
type HC = Z :. Hashed :. OrdCompressed

type COO = Z :. NonUniqueCompressed :. UnsafeCompleteSingleton :. UnsafeCompleteSingleton

type I3 = Z :. Int :. Int :. Int
type CSF3 = Z :. OrdCompressed :. OrdCompressed :. OrdCompressed


main :: Prelude.IO ()
main = let 
          --  vs = [1.0, 3.0, 5.0, 7.0]

           d1s = [2,   2,    2,    1,   1,   8,   24]
           d2s = [3,   1,    0,    4,   1,   5,   3]
           d3s = [1,   2,    2,    3,   4,   5,   6]
           vs  = [2.3, 21.1, 12.0, 1.4, 5.1, 8.5, 24.3]

           ks = zipWith3 (\d1 d2 d3 -> Z_ ::. d1 ::. d2 ::. d3) (use d1s) (use d2s) (use d3s)
           kvs = zip ks (use vs)

           vec = fold (Keep :. Group :. Group) (+) 0 $ createTable @CSF3 @I3 @Float $ kvs


       in  Prelude.print $ run vec
