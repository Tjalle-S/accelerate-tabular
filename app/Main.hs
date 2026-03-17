{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLists #-}

module Main (main) where

import Data.Array.Accelerate
import Data.Array.Accelerate.LLVM.Native

import qualified Prelude

import Data.Array.Accelerate.Tabular.Util
import Data.Array.Accelerate.Tabular.Rep
import Data.Array.Accelerate.Tabular.Classes.Rep

import Data.Array.Accelerate.Unsafe (undef)
import Data.Array.Accelerate.Tabular.Prelude

type Dense2 = Z :.: Dense :.: Dense
type KeyI2  = Z :.: Int   :.: Int

main :: Prelude.IO ()
main = do
  let ks = [Z :.: 0 :.: 1, Z :.: 2 :.: 0, Z :.: 1 :.: 0, Z :.: 2 :.: 1, Z :.: 0 :.: 0, Z :.: 1 :.: 1]
  let vs = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]

  Prelude.print (run $ createTable @Dense2 @KeyI2 @Float (zip (use ks) (use vs)))
