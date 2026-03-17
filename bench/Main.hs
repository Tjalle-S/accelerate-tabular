{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Data.Array.Accelerate ((:.)(..), Acc, Vector)
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.LLVM.Native as CPU

import Data.Array.Accelerate.Tabular.Rep
import Data.Function (on)
import Data.List (sortBy)
import System.Random (randoms, mkStdGen, StdGen)
import Criterion.Main
import Benchmarks.Micro.CreateTable
import System.IO

-- import qualified Prelude

type Dense1 = Z :.: Dense
type Dense2 = Z :.: Dense :.: Dense
type Dense3 = Z :.: Dense :.: Dense :.: Dense
type Dense4 = Z :.: Dense :.: Dense :.: Dense :.: Dense

type KeyI1 = Z :.: Int
type KeyI2 = Z :.: Int :.: Int
type KeyI3 = Z :.: Int :.: Int :.: Int
type KeyI4 = Z :.: Int :.: Int :.: Int :.: Int

main :: IO ()
main = let seed = 42
           gen  = mkStdGen seed
           dat1 = makeData gen (z .:. 10000)
           dat2 = makeData gen (z .:. 125 .:. 80)
           dat3 = makeData gen (z .:. 100 .:. 10 .:. 10)
           dat1' = makeData gen (z .:.: 10000)
           dat2' = makeData gen (z .:.: 125 .:.: 80)
           dat3' = makeData gen (z .:.: 100 .:.: 10 .:.: 10)
       in do
        putStrLn "Compiling accelerate programs"
        hFlush stdout
        let !runA1 = CPU.runN (array @A.DIM1)
        let !runA2 = CPU.runN (array @A.DIM2)
        let !runA3 = CPU.runN (array @A.DIM3)

        putStrLn "Compiling accelerate-tabular programs"
        hFlush stdout
        let !runT1 = CPU.runN (table @Dense1 @KeyI1)
        let !runT2 = CPU.runN (table @Dense2 @KeyI2)
        let !runT3 = CPU.runN (table @Dense3 @KeyI3)

        defaultMain
          [ bgroup "Accelerate"
            [ bench "Dim1" $ nf runA1 dat1
            , bench "Dim2" $ nf runA2 dat2
            , bench "Dim3" $ nf runA3 dat3
            -- , bench "Dim4" $ nf (CPU.run . array) (use dat4)
            ]
          , bgroup "Tabular"
            [ bench "Dim1" $ nf runT1 dat1'
            , bench "Dim2" $ nf runT2 dat2'
            , bench "Dim3" $ nf runT3 dat3'
            -- , bench "Dim4" $ nf (CPU.run . array) (use dat4)
            ]
          ]

z :: [Z]
z = [Z]

makeData :: (A.Elt k) => StdGen -> [k] -> Vector (k, Float)
makeData gen ks = let dat = randoms gen
                  in  A.fromList (Z :. length ks) (shuffle gen $ zip ks dat)

infixl 3 .:.:
(.:.:) :: [key] -> Int -> [key :.: Int]
(.:.:) = addDimension (:.:)

infixl 3 .:.
(.:.) :: [key] -> Int -> [key :. Int]
(.:.) = addDimension (:.)

shuffle :: StdGen -> [a] -> [a]
shuffle gen =
    map snd
  . sortBy (compare `on` fst)
  . zip (randoms @Int gen)

addDimension :: (a -> Int -> b) -> [a] -> Int -> [b]
addDimension f ks n = [f k i | k <- ks, i <- [0 .. n - 1]]
