{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}


import Control.Applicative
import Control.Monad
import qualified Prelude as P
import qualified Data.List as P

import Test.Tasty

import Data.Array.Accelerate hiding (size)
import Data.Array.Accelerate.Trafo.Sharing (Afunction(..))
import Data.Array.Accelerate.Sugar.Shape (size)

import Data.Array.Accelerate.Data.Sort.Merge

import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R
import Test.Tasty.Hedgehog (testProperty)
import Data.Array.Accelerate.LLVM.Native (runN)

type Run  = forall a. Arrays a => Acc a -> a
type RunN = forall f. Afunction f => f -> AfunctionR f

main :: P.IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testProperty "Merge Sort"  $ testMergeSort runN $ G.int (R.linear 0 2048)

  ]

testMergeSort :: (Show e, P.Ord e, Ord e) => RunN -> Gen e -> Property
testMergeSort runN' e = property $ do
  sh <- forAll dim1
  xs <- forAll (array sh e)
  let !go = runN' sort
  go xs === sortRef P.compare xs


sortRef :: Elt a => (a -> a -> Ordering) -> Vector a -> Vector a
sortRef cmp xs = fromList (arrayShape xs) (P.sortBy cmp $ toList xs)

dim0 :: Gen DIM0
dim0 = return Z

dim1 :: Gen DIM1
dim1 = (Z :.) <$> G.int (R.linear 0 1024)

array :: (Shape sh, Elt e) => sh -> Gen e -> Gen (Array sh e)
array sh gen = fromList sh <$> G.list (R.singleton $ size sh) gen
