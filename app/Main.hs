{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Data.Array.Accelerate
import Data.Array.Accelerate.LLVM.Native

import qualified Prelude

import Data.Array.Accelerate.Tabular.Util
import Data.Array.Accelerate.Tabular.Rep
import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Function (on)
import Data.Array.Accelerate.Data.Sort.Quick (sortBy, sort)
import Data.Array.Accelerate.Tabular.Prelude

type Dense3 = Z :.: Dense :.: Dense :.: Dense
type KeyI3  = Z :.: Int   :.: Int   :.: Int


main :: Prelude.IO ()
main = do
  -- let ks = use $ fromList (Z :. 6) [Z :.: 0 :.: 1, Z :.: 2 :.: 0, Z :.: 1 :.: 0, Z :.: 2 :.: 1, Z :.: 0 :.: 0, Z :.: 1 :.: 1] :: Acc (Vector (Z :.: Int :.: Int))
  -- let g = use $ fromList (Z :. 6) [4,0,2,5,1,3]

  Prelude.putStrLn (inspectCompiler @Native (createTable @Dense3 @KeyI3 @Float))

  -- Prelude.print (run $ createTable @Dense2 @KeyI2 (use $ fromList (Z :. 4)[(Z :.: 0 :.: 0, 1 :: Float), (Z :.: 0 :.: 1, 2 :: Float), (Z :.: 1 :.: 0, 3 :: Float), (Z :.: 1 :.: 1, 4 :: Float) ]))



  -- let prog = lift $ createMeta @(Z :.: Dense :.: Dense) @(Z :.: Int :.: Int) ks
  
  -- Prelude.print (run $ gather g ks)
  -- Prelude.putStrLn (inspectCompiler @Native prog)
  -- Prelude.print (run prog)


  -- let xs = map (+1) $ generate (I1 100) (\(I1 i) -> i * 3 + 1)
  -- Prelude.putStrLn (inspectCompiler @Native xs)
  -- Prelude.print (run $ segmentedSort flags vals)


-- table :: (Rep rep key) => Acc (Vector (key, Float)) -> Acc (Table rep key Float)
-- table = createTable

array :: (Shape sh, Ord sh) => Acc (Vector (sh, Float)) -> Acc (Array sh Float)
array kvs = let dim  = maximum (map fst kvs)
                vals = map snd $ sortBy (compare `on` fst) kvs
            in reshape (the dim) vals
