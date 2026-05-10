{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)

import Data.Array.Accelerate.LLVM.Native (runN)

import Data.Array.Accelerate.Tabular
import Properties
import Data.Proxy (Proxy(..))

import Gen


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "Create preserves assocs"  testsCreatePreservesAssocs
  , testGroup "FoldAll equal to foldr"   testsFoldAll
  , testGroup "Filter preserves correct" testsFilter
  , testGroup "Indexing finds correct"   testsIndexing
  ]

testsCreatePreservesAssocs :: [TestTree]
testsCreatePreservesAssocs =
  [ makeTest "Dense / Int"          dense      dim1  int
  , makeTest "Dense / Char"         dense      char1 int
  , makeTest "OrdCompressed / Int"  compressed dim1  int
  , makeTest "OrdCompressed / Char" compressed char1 int
  , makeTest "Hashed / Int"         hashed     dim1  int
  , makeTest "Hashed / Char"        hashed     char1 int
  , makeTest "Dense2 / Int:.Int"    dense2     dim2  int
  , makeTest "Dense2 / Int:.Char"   dense2     char2 int
  , makeTest "Hashed2 / Int:.Int"   hashed2    dim2  int
  , makeTest "CSR / Int:.Int"       csr        dim2  int
  , makeTest "COO / Int:.Int"       coo        dim2  int
  , makeTest "CSF / Int:.Int:.Int"  csf        dim3  int
  ]
  where
    makeTest name proxy key val = testProperty name $
      createPreservesAssocs runN proxy (genAssocs key val)

testsFoldAll :: [TestTree]
testsFoldAll =
  [ makeTest "Dense / Int"          dense      dim1  int
  , makeTest "OrdCompressed / Int"  compressed dim1  int
  , makeTest "Hashed / Int"         hashed     dim1  int
  , makeTest "Dense2 / Int:.Int"    dense2     dim2  int
  , makeTest "Hashed2 / Int:.Int"   hashed2    dim2  int
  , makeTest "CSR / Int:.Int"       csr        dim2  int
  , makeTest "COO / Int:.Int"       coo        dim2  int
  , makeTest "CSF / Int:.Int:.Int"  csf        dim3  int
  ]
  where
    makeTest name proxy key val = testProperty name $
      foldAllSameAsFoldr runN proxy (genAssocs key val)

testsFilter :: [TestTree]
testsFilter =
  [ makeTest "Dense / Int"          dense      dim1  int
  , makeTest "Dense / Char"         dense      char1 int
  , makeTest "OrdCompressed / Int"  compressed dim1  int
  , makeTest "OrdCompressed / Char" compressed char1 int
  , makeTest "Hashed / Int"         hashed     dim1  int
  , makeTest "Hashed / Char"        hashed     char1 int
  , makeTest "Dense2 / Int:.Int"    dense2     dim2  int
  , makeTest "Dense2 / Int:.Char"   dense2     char2 int
  , makeTest "Hashed2 / Int:.Int"   hashed2    dim2  int
  , makeTest "CSR / Int:.Int"       csr        dim2  int
  , makeTest "COO / Int:.Int"       coo        dim2  int
  , makeTest "CSF / Int:.Int:.Int"  csf        dim3  int
  ]
  where
    makeTest name proxy key val = testProperty name $
      filterPreservesFiltered runN proxy (genAssocs key val)

testsIndexing :: [TestTree]
testsIndexing =
  [ makeTest "Dense / Int"          dense      dim1  int
  , makeTest "Dense / Char"         dense      char1 int
  , makeTest "OrdCompressed / Int"  compressed dim1  int
  , makeTest "OrdCompressed / Char" compressed char1 int
  , makeTest "Hashed / Int"         hashed     dim1  int
  , makeTest "Hashed / Char"        hashed     char1 int
  , makeTest "Dense2 / Int:.Int"    dense2     dim2  int
  , makeTest "Dense2 / Int:.Char"   dense2     char2 int
  , makeTest "Hashed2 / Int:.Int"   hashed2    dim2  int
  , makeTest "CSR / Int:.Int"       csr        dim2  int
  , makeTest "COO / Int:.Int"       coo        dim2  int
  , makeTest "CSF / Int:.Int:.Int"  csf        dim3  int
  ]
  where
    makeTest name proxy key val = testProperty name $
      indexingSameAsLookup runN proxy (genAssocs key val) (`array` key)

dense :: Proxy (Z :. Dense)
dense = Proxy

compressed :: Proxy (Z :. OrdCompressed)
compressed = Proxy

hashed :: Proxy (Z :. Hashed)
hashed = Proxy

dense2 :: Proxy (Z :. Dense :. Dense)
dense2 = Proxy

csr :: Proxy (Z :. Dense :. OrdCompressed)
csr = Proxy

coo :: Proxy (Z :. NonUniqueCompressed :. UnsafeCompleteSingleton)
coo = Proxy

csf :: Proxy (Z :. OrdCompressed :. OrdCompressed :. OrdCompressed)
csf = Proxy

hashed2 :: Proxy (Z :. Hashed :. OrdCompressed)
hashed2 = Proxy
