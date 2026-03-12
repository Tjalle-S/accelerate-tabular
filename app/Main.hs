{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Array.Accelerate
import Data.Array.Accelerate.LLVM.Native

import qualified Prelude

main :: Prelude.IO ()
main = do
  Prelude.putStrLn (inspectCompiler @Native (split (even @Int)))
  -- let xs = map (+1) $ generate (I1 100) (\(I1 i) -> i * 3 + 1)
  -- Prelude.putStrLn $ inspectCompiler @Native xs
  -- Prelude.putStrLn $ Prelude.show $ run xs

split :: (Elt a)
      => (Exp a -> Exp Bool)
      -> Acc (Vector a)
      -> Acc (Vector a, Vector a)
split f xs = lift (afst $ filter f xs, afst $ filter (not . f) xs)
