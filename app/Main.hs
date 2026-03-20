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

main :: Prelude.IO ()
main = let 
          --  vs = [1.0, 3.0, 5.0, 7.0]

           d1s = [2, 2, 1, 4, 0, 0, 0]
           d2s = [3, 1, 0, 4, 1, 5, 3]
           vs  = [2.3, 2.1, 1.0, 4.4, 0.1, 0.5, 0.3]

           ks = zipWith (\d1 d2 -> Z_ ::.: d1 ::.: d2) (use d1s) (use d2s)
           kvs = zip ks (use vs)

           vec = createTable @CSR @I2 @Float kvs


       in  Prelude.print $ run vec
