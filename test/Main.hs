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
  , testGroup "Inner join matches ref"   testsInnerJoin
  ]

testsCreatePreservesAssocs :: [TestTree]
testsCreatePreservesAssocs =
  [ makeTest "Dense / Int"          dense      dim1  int
  , makeTest "Dense / Char"         dense      char1 int
  , makeTest "Compressed / Int"  compressed dim1  int
  , makeTest "Compressed / Char" compressed char1 int
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
  , makeTest "Compressed / Int"  compressed dim1  int
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
  , makeTest "Compressed / Int"  compressed dim1  int
  , makeTest "Compressed / Char" compressed char1 int
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
  , makeTest "Compressed / Int"  compressed dim1  int
  , makeTest "Compressed / Char" compressed char1 int
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

testsInnerJoin :: [TestTree]
testsInnerJoin =
  [ makeTest "Dense / Int"          dense dense dense      dim1  int
  , makeTest "Compressed / Int"  compressed compressed compressed dim1  int
  , makeTest "Hashed / Int"         hashed hashed hashed    dim1  int
  , makeTest "Dense2/CSR/Dense2"    dense2 csr dense2     dim2  int
  , makeTest "CSR/Dense2/Hashed2"    csr dense2 hashed2     dim2  int
  ]
  where
    makeTest name proxy1 proxy2 proxy3 key val = testProperty name $
      innerJoinReference runN proxy1 proxy2 proxy3 (genAssocs key val)

dense :: Proxy (Z :. Dense)
dense = Proxy

compressed :: Proxy (Z :. Compressed)
compressed = Proxy

hashed :: Proxy (Z :. Hashed)
hashed = Proxy

dense2 :: Proxy (Z :. Dense :. Dense)
dense2 = Proxy

csr :: Proxy (Z :. Dense :. Compressed)
csr = Proxy

coo :: Proxy Coo
coo = Proxy

csf :: Proxy (Z :. Compressed :. Compressed :. Compressed)
csf = Proxy

hashed2 :: Proxy (Z :. Hashed :. Compressed)
hashed2 = Proxy
