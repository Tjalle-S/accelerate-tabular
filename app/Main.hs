{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Data.Array.Accelerate
import Data.Array.Accelerate.LLVM.Native
import Data.Array.Accelerate.Tabular

import qualified Prelude

main :: Prelude.IO ()
main = do
  let tab = project (chr . subtract 32 . ord) testTable3
  -- let val = unsafeIndex' 0 tab (Z_ ::.: 1 ::.: 12)
  Prelude.putStrLn $ test @UniformScheduleFun @NativeKernel (enumKeys $ meta_ tab)
  Prelude.print (run tab)
  Prelude.print $ run (enumKeys $ meta_ tab)

  -- let xs = map (+1) $ generate (I1 100) (\(I1 i) -> i * 3 + 1)
  -- Prelude.putStrLn $ Prelude.show $ run xs
