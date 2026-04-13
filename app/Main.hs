{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Prelude

import Data.Array.Accelerate (zip, zipWith, foldSeg)
import Data.Array.Accelerate.LLVM.Native
import Data.Array.Accelerate.Tabular.Rep
import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular
import Data.Array.Accelerate.Tabular.Rep.Grouped
import Data.Array.Accelerate.Tabular.Classes.Fold
import Data.Array.Accelerate.Tabular.Prelude.Table
import Data.Proxy (Proxy (Proxy))

type I2 = Z :.: Int :.: Int
type D2 = Z :.: Dense :.: Dense
type CSR = Z :.: Dense ::: OrdCompressed
type CSF = Z :.: OrdCompressed ::: OrdCompressed

type CD = Z :.: OrdCompressed :.: Dense

type I1 = Z :.: Int
type Sparse = Z :.: OrdCompressed

type H = Z :.: Hashed
type HC = Z :.: Hashed :.: OrdCompressed

type COO = Z :.: NonUniqueCompressed :.: UnsafeCompleteSingleton :.: UnsafeCompleteSingleton

type I3 = Z :.: Int :.: Int :.: Int
type CSF3 = Z :.: OrdCompressed :.: OrdCompressed :.: OrdCompressed

type D2' = Z :.: Dense ::: Dense

main :: Prelude.IO ()
main = let 
          --  vs = [1.0, 3.0, 5.0, 7.0]

           d1s = [2,   2,    2,    1,   1,   8,   24]
           d2s = [3,   1,    0,    4,   1,   5,   3]
        --    d3s = [1,   2,    2,    3,   4,   5,   6]
           vs  = [2.3, 21.1, 12.0, 1.4, 5.1, 8.5, 24.3]

           ks = zipWith (\d1 d2 -> Z_ ::.: d1 ::.: d2) (use d1s) (use d2s)
           kvs = zip ks (use vs)

           (met, _, _) = createMeta @D2' @I2 ks
           (_, seg) = foldMeta met

           vec = fold' (Keep :.: Group :.: Group) (+) 0 $ createTable @D2 @I2 @Float $ kvs


       in  Prelude.print $ run vec


-- | Reduction of a table of arbitrary dimensionality.
-- The first argument needs to be function that is both associative /and/ commutative.
fold' :: (Fold''' rep key, Elt val, FoldDescriptor rep key desc)
      => desc
      -> (Exp val -> Exp val -> Exp val)
      -> Exp val
      -> Acc (Table rep key val)
      -> Acc (Table (FoldResult' rep desc) (FoldResult' key desc) val)
fold' d f e Table_ { meta_, vals_ } = 
  let T2 met' seg = foldMeta''' (getDescriptor $ Prelude.pure d) meta_
  in  Table_ met' $ foldSeg (combineMaybe f) (Just_ e) vals_ seg

combineMaybe :: Elt a
             => (Exp a -> Exp a -> Exp a)
             -> Exp (Maybe a)
             -> Exp (Maybe a)
             -> Exp (Maybe a)
combineMaybe f mx my = T2 mx my & match \case
  T2 Nothing_  Nothing_  -> Nothing_
  T2 (Just_ x) Nothing_  -> Just_ x
  T2 Nothing_  (Just_ y) -> Just_ y
  T2 (Just_ x) (Just_ y) -> Just_ (f x y)
