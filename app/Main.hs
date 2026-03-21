{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import qualified Prelude

import Data.Array.Accelerate
import Data.Array.Accelerate.LLVM.Native
import Data.Array.Accelerate.Data.Sort.Quick

import Data.Function (on)
import Data.Array.Accelerate.Tabular.Rep
import Data.Array.Accelerate.Tabular.Prelude
import Data.Array.Accelerate.Tabular.Util
import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Data.Semigroup
import Data.Array.Accelerate.Unsafe (undef)

type I2 = Z :.: Int :.: Int
type CSR = Z :.: Dense :.: OrdCompressed
type CSF = Z :.: OrdCompressed :.: OrdCompressed

type CD = Z :.: OrdCompressed :.: Dense

type I1 = Z :.: Int
type Sparse = Z :.: OrdCompressed

type H = Z :.: Hashed
type HC = Z :.: Hashed :.: OrdCompressed

main :: Prelude.IO ()
main = let 
          --  vs = [1.0, 3.0, 5.0, 7.0]

           d1s = [2,   12,   12,   2,   5,   8,   24]
           d2s = [3,   1,    0,    4,   1,   5,   3]
           vs  = [2.3, 12.1, 12.0, 2.4, 5.1, 8.5, 24.3]

           ks = zipWith (\d1 d2 -> Z_ ::.: d1 ::.: d2) (use d1s) (use d2s)
           kvs = zip ks (use vs)

           vec = createTable @HC @I2 @Float $ kvs


       in  Prelude.print $ run vec
