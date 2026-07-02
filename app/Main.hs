{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Main (main) where

import qualified Prelude

import Data.Array.Accelerate.LLVM.Native
import Data.Array.Accelerate.Tabular
import Data.Array.Accelerate.Tabular.Prelude.Table (toArray)
import Data.Array.Accelerate (inspectCompiler)
import Data.Array.Accelerate.Tabular.Prelude.Assocs (assocs')

type I1 = Z :. Int
type I2 = Z :. Int :. Int

type D1 = Z :. Dense
type D2 = Z :. Dense :. Dense



main :: Prelude.IO ()
main = let prog = assocs' . apsp @(Z :. Dense :. Dense)
       in  do Prelude.putStrLn $ inspectCompiler @Native prog
              Prelude.print $ run $ prog distances

type Key = Z :. Int :. Int

inf :: Exp Float
inf = 1 / 0

apsp :: forall rep. (Slice rep Key)
     => Acc (Table rep Key Float)
     -> Acc (Table rep Key Float)
apsp ds = afor (unit n) update ds
  where
    Z_ ::. n' ::. n'' = the $ fold1All max $ keys ds
    n = max n' n''

    update ak d = 
      let k = the ak
          toK   = slice @(Z :. Dense) (Z_ ::. Keep_    ::. Slice_ k) d
          fromK = slice @(Z :. Dense) (Z_ ::. Slice_ k ::. Keep_)    d

          added = cartesianWith @rep (+) toK fromK
      in  fullOuterJoin @rep min inf inf d added

distances :: (Rep rep Key) => Acc (Table rep Key Float)
distances = createTable (use ds)
  where
    ds = [ (Z :. 0 :. 1, 2)
         , (Z :. 0 :. 2, 4)
         , (Z :. 1 :. 3, 6)
         , (Z :. 2 :. 3, 8)
         ]


--  0
-- / \
-- 1 2
-- \ /
--  3


afor :: (Arrays a) => Acc (Scalar Int)
                   -> (Acc (Scalar Int) -> Acc a -> Acc a)
                   -> Acc a
                   -> Acc a
afor n f x = asnd $ awhile
  (\(T2 i _)  -> unit (the i < the n))
  (\(T2 i x') -> T2 (map (+ 1) i) (f i x'))
  (T2 (unit 0) x)
