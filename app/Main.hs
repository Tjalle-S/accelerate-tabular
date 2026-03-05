{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Data.Array.Accelerate
import Data.Array.Accelerate.LLVM.Native

import qualified Prelude

import Data.Array.Accelerate.Tabular.Util
import Data.Array.Accelerate.Tabular.Rep
import Data.Array.Accelerate.Tabular.Classes.Rep

main :: Prelude.IO ()
main = do
  let ks = use $ fromList (Z :. 6) [Z :.: 0 :.: 1, Z :.: 2 :.: 0, Z :.: 1 :.: 0, Z :.: 2 :.: 1, Z :.: 0 :.: 0, Z :.: 1 :.: 1] :: Acc (Vector (Z :.: Int :.: Int))
  let g = use $ fromList (Z :. 6) [4,0,2,5,1,3]

  -- let prog = lift $ createMeta @(Z :.: Dense :.: Dense) @(Z :.: Int :.: Int) ks
  
  Prelude.print (run $ gather g ks)
  -- Prelude.putStrLn (inspectCompiler @Native prog)
  -- Prelude.print (run prog)


  -- let xs = map (+1) $ generate (I1 100) (\(I1 i) -> i * 3 + 1)
  -- Prelude.putStrLn (inspectCompiler @Native xs)
  -- Prelude.print (run $ segmentedSort flags vals)
