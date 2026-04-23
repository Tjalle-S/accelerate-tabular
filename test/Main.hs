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
import Test.Tasty.Hedgehog (testProperty)


import Data.Array.Accelerate hiding (size)
import Data.Array.Accelerate.LLVM.Native (runN)
import Data.Array.Accelerate.Tabular
import Properties
import Data.Proxy (Proxy(..))

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
  , testProperty "Dense / Char" $
      createPreservesAssocs
        runN
        (Proxy @(Z :. OrdCompressed))
        (genAssocs char1 int)
  , testProperty "OrdCompressed / Int" $
      createPreservesAssocs
        runN
        (Proxy @(Z :. OrdCompressed))
        (genAssocs dim1 int)
  , testProperty "OrdCompressed / Char" $
      createPreservesAssocs
        runN
        (Proxy @(Z :. OrdCompressed))
        (genAssocs char1 int)
  , testProperty "Hashed / Int" $
      createPreservesAssocs
        runN
        (Proxy @(Z :. Hashed))
        (genAssocs dim1 int)
  , testProperty "Hashed / Char" $
      createPreservesAssocs
        runN
        (Proxy @(Z :. Hashed))
        (genAssocs char1 int)
  , testProperty "Dense2 / Int:.Int" $
      createPreservesAssocs
        runN
        (Proxy @(Z :. Dense :. Dense))
        (genAssocs dim2 int)
  , testProperty "Dense2 / Int:.Char" $
      createPreservesAssocs
        runN
        (Proxy @(Z :. Dense :. Dense))
        (genAssocs ((\a b -> Z :. a :. b) <$> intKey <*> charKey) int)
  , testProperty "CSR / Int:.Int" $
      createPreservesAssocs
        runN
        (Proxy @(Z :. Dense :. OrdCompressed))
        (genAssocs dim2 int)
  , testProperty "COO / Int:.Int" $
      createPreservesAssocs
        runN
        (Proxy @(Z :. NonUniqueCompressed :. UnsafeCompleteSingleton))
        (genAssocs dim2 int)
  , testProperty "CSF / Int:.Int:.Int" $
      createPreservesAssocs
        runN
        (Proxy @(Z :. OrdCompressed :. OrdCompressed :. OrdCompressed))
        (genAssocs dim3 int)
  ]
  where
    char1 = (Z :.) <$> charKey