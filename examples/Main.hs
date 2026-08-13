{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ParallelListComp #-}

module Main (main) where

import Prelude (IO, String, fromIntegral, return , (++), readFile, lines, (<$>), putStrLn, splitAt, read, FilePath)
import qualified Prelude as P

import System.CPUTime
import Text.Printf
import Data.Array.Accelerate.LLVM.Native
-- import Data.Array.Accelerate.Interpreter.Simple

import Data.Array.Accelerate hiding (fromIntegral, (++))--(Vector, Matrix, DIM2, Double, Float, ($), undefined, unzip, permuteUnique')
import Data.Array.Accelerate.Tabular (Dense, Compressed, createTable, Table (vals))

import qualified APSP.Accelerate.Dense as Dense
import qualified APSP.Accelerate.CSR   as CSR
import qualified APSP.Tabular          as Tabular
import qualified Quickhull.Accelerate  as QA
import qualified Quickhull.Tabular     as TA
import Quickhull.Common

import Data.Data
import Criterion
import Criterion.Main

import Control.DeepSeq
import GHC.Exts (IsList (Item))
import qualified GHC.Exts as GHC
import Lens.Micro
import System.Exit (exitSuccess)
import Debug.Trace
import System.Environment (getArgs, withArgs)
import System.IO (hFlush, stdout)

import Criterion.Measurement hiding (getCPUTime)
import Criterion.Measurement.Types (Measured (measTime))

type Dense1 = Z :. Dense

main :: IO ()
main = do
  -- (file : rest) <- getArgs
  -- dat <- makeCooGraph file :: IO (Vector (DIM2, Float))
  -- P.print dat
  dat1 <- makeCooGraph "data/graph.in"
  dat2 <- makeCooGraph "data/graph.in"

  let !dat1' = force dat1
  let !dat2' = force dat2

  -- !datTab <- makePoints file
  -- P.print dat

  -- exitSuccess

  !progDense <- timeIt "Dense" $ runN (Dense.apsp . toMatrix)
  !progCSR   <- timeIt "CSR"   $ runN (CSR.apsp . CSR.makeCSR)

  progTabDense <- timeIt "Tab:Dense"
    $ runN
    $ (Tabular.apsp @(Dense1 :. Dense) @Dense1 Proxy) . createTable
  progTabCSR <- timeIt "Tab:CSR"
    $ runN
    $ (Tabular.apsp @(Dense1 :. Compressed) @Dense1 Proxy) . createTable
  
  -- !progQuickhullAcc <- timeIt "Accelerate" $ runN QA.quickhull
  -- !progQuickhullTab <- timeIt "Tabular"    $ runN TA.quickhull

  -- !resAcc <- timeIt "Run: Accelerate" $ progQuickhullAcc dat'
  -- !resTab <- timeIt "Run: Tabular"    $ progQuickhullTab (force datTab)

  -- P.print (arraySize resAcc)
  -- P.print resTab

  !resDense <- timeIt "Run: Dense" $ (progDense dat1')
  !resCSR   <- timeIt "Run: CSR"   $ (progCSR   dat1')

  !resTabDense <- timeIt "Run: Tab Dense" $ progTabDense dat2'
  !resTabCSR   <- timeIt "Run: Tab CSR"   $ progTabCSR   dat2'


  P.print (arraySize resDense)
  P.print (arraySize $ P.snd resCSR)
  P.print (arraySize $ vals resTabDense)
  P.print (arraySize $ vals resTabCSR)

  -- let !progQuickhull = arraySize . runN QA.quickhull
  -- P.print $ arraySize $ P.snd $ progCSR dat

  -- test "Dense" progDense dat
  -- test "CSR"   progCSR   dat

  


  -- withArgs rest $
  --   defaultMain [
  --     bgroup "Accelerate" [
  --       env (makeEnv file "Dense" progDense) $ \(~(d, p)) -> 
  --         bench "Dense" $ nf p d
  --     , env (makeEnv file "CSR" progCSR) $ \(~(d, p)) ->
  --         bench "CSR" $ nf p d
  --     ]
  --   ]
      --    -> bgroup "Accelerate" [
      --       bench "Dense" $ nf (arraySize . progDense) d
      --     , bench "CSR"   $ nf (arraySize . P.snd . progCSR)   d
      --     ]
      -- , env (makeCoo file) $ \d -> bgroup "Tabular" [
      --     bench "Dense" $ nf (arraySize . vals . progTabDense) d
      --   , bench "CSR"   $ nf (arraySize . vals . progTabCSR)   d
      --   ]
      -- ]
  -- withArgs rest $
  --   defaultMain [
  --       env (makeCoo file) $ \d -> bgroup "Accelerate" [
  --           bench "Dense" $ nf (arraySize . progDense) d
  --         , bench "CSR"   $ nf (arraySize . P.snd . progCSR)   d
  --         ]
  --     , env (makeCoo file) $ \d -> bgroup "Tabular" [
  --         bench "Dense" $ nf (arraySize . vals . progTabDense) d
  --       , bench "CSR"   $ nf (arraySize . vals . progTabCSR)   d
  --       ]
  --     ]

timeIt :: NFData a => String -> a -> IO a
timeIt name x = do
  t1 <- getCPUTime
  let !y = force x
  t2 <- getCPUTime
  let t = (fromIntegral (t2 - t1) :: Double) * 1e-12
  printf (name ++ ":\n%6.2fs\n") t
  hFlush stdout
  return y

chunk :: [Int] -> [a] -> [[a]]
chunk [] _ = []
chunk (n : ns) xs = let (xs', ys) = splitAt n xs
                    in  xs' : chunk ns ys

makeCooGraph :: (IsList l, Item l ~ (DIM2, Float)) => FilePath -> IO l
makeCooGraph file = do
  (n : m : dat) <- P.map read . P.tail . lines <$> readFile file
  let (seg, edges) = splitAt n dat
      ls = P.zipWith (P.subtract) seg (P.tail seg P.++ [m])
      edges' = chunk ls edges
      coo = P.map (, 1) [ (Z :. s :. e)
                        | s  <- [0..]
                        | es <- edges'
                        , e  <- es
                        ]

  return $ GHC.fromListN m coo

makePoints :: (IsList l, Item l ~ Point) => FilePath -> IO l
makePoints file = do
  ("pbbs_sequencePoint2d" : rest) <- lines <$> readFile file
  return $ GHC.fromList (P.map parsePoint rest)
  where
    parsePoint l =
      let [x, y] = P.words l
      in  (read x, read y)

makeEnv :: (IsList l, Item l ~ (DIM2, Float)) => FilePath -> String -> (l -> Int) -> IO (l, l -> Int)
makeEnv file name func = do
  dat <- makeCooGraph file
  !func' <- timeIt name func
  return (dat, func')

  -- return $ GHC.fromList [
  --   (Z:.0:.1, 1)
  -- , (Z:.1:.3, 5)
  -- ]

instance NFData Z
instance (NFData a, NFData b) => NFData (a :. b)

toMatrix :: Acc (Vector (DIM2, Float)) -> Acc (Matrix Float)
toMatrix kvs = let sz     = the $ maximum $ map fst kvs
                   target = fill sz 0
               in permuteUnique' target $ map Just_ kvs

test name p d = do
  P.putStrLn name
  (measured, endtime) <- measure (nf p d) 5
  P.putStrLn (secs $ measTime measured / 5)
