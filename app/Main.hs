{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Prelude

import Data.Array.Accelerate
import Data.Array.Accelerate.LLVM.Native
import Data.Array.Accelerate.Data.Sort.Quick

import Data.Function (on)

main :: Prelude.IO ()
main = Prelude.putStrLn (inspectCompiler @Native (test @Int))

test :: Ord a
     => Acc (Vector a)
     -> Acc (Vector DIM1)
test is =
  let perm = fill (shape is) (I1 0)

      (_, perm') = unzip
        $ sortBy (compare `on` fst)
        $ zipChecked perm (enumFromN (shape is) 0)

      perm'' = gather (map unindex1 perm) (map I1 perm')
  in  perm''
