{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Array.Accelerate
import Data.Array.Accelerate.LLVM.Native
import Data.Array.Accelerate.Tabular

import qualified Prelude

main :: Prelude.IO ()
main = do
  let tab = project (chr . subtract 32 . ord) testTable2
  -- let val = unsafeIndex' 0 tab (Z_ ::.: 1 ::.: 12)
  Prelude.print (run $ enumKeys tab)
  -- let xs = map (+1) $ generate (I1 100) (\(I1 i) -> i * 3 + 1)
  Prelude.putStrLn $ test @UniformScheduleFun @NativeKernel (enumKeys tab)
  -- Prelude.putStrLn $ Prelude.show $ run xs
