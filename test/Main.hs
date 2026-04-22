-- {-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}


import Control.Applicative
import Control.Monad
import qualified Prelude as P
import qualified Data.List as P

import Test.Tasty

import Data.Array.Accelerate hiding (size)

import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R
import Test.Tasty.Hedgehog (testProperty)
import Data.Array.Accelerate.LLVM.Native (runN)
import Data.Array.Accelerate.Tabular
import Properties
import Data.Proxy (Proxy(Proxy))
import Data.Array.Accelerate.Tabular.Prelude.Table (NotScalarConstruct)
import Data.Kind

import Gen


main :: P.IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "Create preserves assocs" testsCreatePreservesAssocs

  ]

testsCreatePreservesAssocs :: [TestTree]
testsCreatePreservesAssocs = 
  [ testProperty "Dense / Int" $
      createPreservesAssocs
        runN
        (Proxy @(Z :. Dense))
        (genAssocs dim1 int)
  , testProperty "OrdCompressed / Int" $
      createPreservesAssocs
        runN
        (Proxy @(Z :. OrdCompressed))
        (genAssocs dim1 int)
  , testProperty "Hashed / Int" $
      createPreservesAssocs
        runN
        (Proxy @(Z :. Hashed))
        (genAssocs dim1 int)
  ]
