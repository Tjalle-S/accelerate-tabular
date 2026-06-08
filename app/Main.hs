{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE StandaloneKindSignatures #-}
-- {-# LANGUAGE ConstraintKinds #-}
-- {-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- {-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main (main) where

import qualified Prelude

import Data.Array.Accelerate (Arrays)
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.Native hiding (Arrays)
import Data.Array.Accelerate.Tabular.Rep
import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular hiding (Assert)

-- import qualified Data.Array.Accelerate.Tabular.Prelude.Zip as Z
-- import Data.Array.Accelerate.Tabular.Util (lookupMany)
import Data.Array.Accelerate.Tabular.Prelude
-- import Data.Array.Accelerate.Tabular.Classes.Slice
-- import Data.Array.Accelerate.Tabular.Prelude.Slice
-- import Data.Array.Accelerate.Tabular.Classes.Fold
import Data.Array.Accelerate (inspectCompiler)
import Data.Array.Accelerate.Tabular.Prelude.Cartesian (cartesianWith)
-- import Data.Kind
-- import GHC.TypeError
-- import Data.Data
-- import Data.Array.Accelerate.Tabular.Util
-- import Data.Array.Accelerate.Tabular.Classes.Rep (Rep(orderedCreateMeta))

-- import Data.Array.Accelerate.Sugar.Array
-- import qualified Data.Array.Accelerate.Representation.Array as R
-- import Data.Array.Accelerate.Representation.Type (TupR(TupRpair))
-- import Unsafe.Coerce (unsafeCoerce)
-- import Control.Applicative (Const)

-- type I2 = Z :. Int :. Int
type D2 = Z :. Dense :. Dense

-- type CD = Z :. OrdCompressed :. Dense

-- type I1 = Z :. Int
-- type Sparse = Z :. OrdCompressed

-- type H = Z :. Hashed
-- type HC = Z :. Hashed :. OrdCompressed

-- type COO = Z :. NonUniqueCompressed :. UnsafeCompleteSingleton :. UnsafeCompleteSingleton

-- type I3 = Z :. Int :. Int :. Int
-- type CSF3 = Z :. OrdCompressed :. OrdCompressed :. OrdCompressed


main :: Prelude.IO ()
main = Prelude.print $ run $ apsp @(Z :. Dense :. OrdCompressed) distances
  -- let kvs = use [(Z :. 0 :. 0, 0.0), (Z :. 0 :. 1, 0.1), (Z :. 1 :. 0, 1.0), (Z :. 1 :. 1, 1.1)]
  -- in  Prelude.print $ run $ slice (Z_ ::. Keep_ ::. Slice_ 1) $ createTable @(Z :. Dense :. Dense) @(Z :. Int :. Int) @Float kvs

type Key = Z :. Int :. Int

inf :: Exp Float
inf = 1 / 0

apsp :: forall rep. (Slice rep Key) => Acc (Table rep Key Float) -> Acc (Table rep Key Float)
apsp ds = afor (A.unit n) update ds
  where
    Z_ ::. n' ::. n'' = A.the $ A.maximum $ keys ds
    n = max n' n''

    update :: Acc (A.Scalar Int) -> Acc (Table rep Key Float) -> Acc (Table rep Key Float)
    update ak d = let k = A.the ak
                      toK   = A.compute $ slice @(Z :. Dense) (Z_ ::. Keep_    ::. Slice_ k) d
                      fromK = A.compute $ slice @(Z :. Dense) (Z_ ::. Slice_ k ::. Keep_)    d

                      added = A.compute $ cartesianWith @rep (+) toK fromK
                  in  fullouterjoin @rep min inf inf d added

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


afor :: (Arrays a) => Acc (A.Scalar Int)
                   -> (Acc (A.Scalar Int) -> Acc a -> Acc a)
                   -> Acc a
                   -> Acc a
afor n f x = asnd $ awhile
  (\(T2 i _)  -> A.zipWith (<) i n)
  (\(T2 i x') -> T2 (A.map (+ 1) i) (f i x'))
  (T2 (A.unit 0) x)
