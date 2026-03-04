{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.Array.Accelerate
import Data.Array.Accelerate.LLVM.Native

import Data.Array.Accelerate.Tabular.Prelude
import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Rep

import qualified Prelude
import Data.Array.Accelerate.Tabular.Classes.Index

main :: Prelude.IO ()
main = do
  -- let xs = map (+1) $ generate (I1 100) (\(I1 i) -> i * 3 + 1)
  Prelude.putStrLn $ inspectCompiler @Native (floydWarshall dist)
  Prelude.print (run $ floydWarshall dist)

type Key = Z :.: Int :.: Int

inf :: Float
inf = 999999

dist :: Acc (Matrix Float)
dist = use $ fromList (Z :. 4 :. 4) [
    0,   10,  20,  inf
  , 10,  0,   inf, 40
  , 20,  inf, 0,   inf
  , inf, 40,  inf, 0
  ]

apsp :: (Rep rep Key) => Acc (Table rep Key Float) -> Acc (Table rep Key Float)
apsp ds = afor (unit n) update ds
  where
    Z_ ::.: n' ::.: n'' = the $ maximum $ flatten $ keys ds
    n = max n' n''

    update ak d = let k = the ak
                      toK   = filterTable (matches2 k . fst) d
                      fromK = filterTable (matches1 k . fst) d
                  in  eqJoinWith min d (eqJoinWith (+) toK fromK)

    matches1 k (Z_ ::.: k' ::.: _)  = k == k'
    matches2 k (Z_ ::.: _  ::.: k') = k == k'


eqJoinWith :: (Rep rep key, Elt a, Elt b, Elt c, Eq key)
           => (Exp a -> Exp b -> Exp c)
           -> Acc (Table rep key a)
           -> Acc (Table rep key b)
           -> Acc (Table rep key c)
eqJoinWith = undefined

filterTable :: (Rep rep key, Elt val)
            => (Exp (key, val) -> Exp Bool)
            -> Acc (Table rep key val)
            -> Acc (Table rep key val)
filterTable = undefined

foldAllTable :: (Rep rep key, Elt val)
             => (Exp val -> Exp val -> Exp val)
             -> Acc (Table rep key val)
             -> Acc (Scalar val)
foldAllTable = undefined

indexDefault :: (Index rep key, Elt val) => Exp val -> Acc (Table rep key val) -> Exp key -> Exp val
indexDefault = undefined

-- | Gives an array of keys present in the table.
-- Does not necessarily correspond directly with the vector of values.
keys :: (Rep rep key, Elt val) => Acc (Table rep key val) -> Acc (Vector key)
keys = undefined

-- One Floyd–Warshall step for fixed k
fwStep :: Acc (Scalar Int) -> Acc (Matrix Float) -> Acc (Matrix Float)
fwStep ak mat =
  let
    k = the ak

    col = slice mat (Z_ ::. All_ ::. k)
    row = slice mat (Z_ ::. k ::. All_)

    viaK = outerSum col row

  in zipWith min mat viaK

outerSum :: (Num a) => Acc (Vector a) -> Acc (Vector a) -> Acc (Matrix a)
outerSum xs ys = 
  let xs' = replicate (Z_ ::. All_ ::. length ys) xs
      ys' = replicate (Z_ ::. length xs ::. All_) ys
  in zipWith (+) xs' ys'


-- Full APSP
floydWarshall :: Acc (Matrix Float) -> Acc (Matrix Float)
floydWarshall mat =
  let Z_ ::. n ::. _ = shape mat
  in afor (unit n) fwStep mat

for :: Elt a => Exp Int -> (Exp Int -> Exp a -> Exp a) -> Exp a -> Exp a
for n f x = snd $ while
  (\(T2 i _)  -> i < n)
  (\(T2 i x') -> T2 (i + 1) (f i x'))
  (T2 0 x)

afor :: (Arrays a) => Acc (Scalar Int)
                       -> (Acc (Scalar Int) -> Acc a -> Acc a)
                       -> Acc a
                       -> Acc a
afor n f x = asnd $ awhile
  (\(T2 i _)  -> zipWith (<) i n)
  (\(T2 i x') -> T2 (map (+ 1) i) (f i x'))
  (T2 (unit 0) x)

aiterate :: (Arrays a) => Acc (Scalar Int)
                       -> (Acc a -> Acc a)
                       -> Acc a
                       -> Acc a
aiterate n f x = asnd $ awhile
  (\(T2 i _)  -> zipWith (<) i n)
  (\(T2 i x') -> T2 (map (+ 1) i) (f x'))
  (T2 (unit 0) x)
  