{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module APSP.Accelerate.CSR (apsp, makeCSR) where

import Common
import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Maybe
import Data.Array.Accelerate.Data.Sort.Merge

type CSRMatrix a = (Segments Int, Vector (Int, a))

apsp :: Acc (CSRMatrix Float) -> Acc (CSRMatrix Float)
apsp ds = aforArr (unit n) update ds
  where
    n' = length (afst ds)
    n'' = the $ maximum (map fst $ asnd ds)
    n = max n' n''

    update ak (T2 seg kvs) =
      let k = the ak

          origKvs =
            let segBounds = scatter seg (fill (shape kvs) 0) (fill (shape seg) 1)
                ks = postscanl (+) 0 segBounds
             in map (\(T2 k1 (T2 k2 v)) -> T2 (I2 k1 k2) v) (zip ks kvs)

          toK =
            let kvs' = filter (\(T2 (I2 _ k') _) -> k' == k) origKvs
             in map (\(T2 (I2 k' _) v) -> T2 k' v) (afst kvs')
          fromK =
            let start = if k == 0 then 0 else seg !! (k - 1)
                end = seg !! k
                range = enumFromN (I1 $ end - start) start
             in gather range kvs

          viaK =
            let toK' = replicate (Z_ ::. All_ ::. length fromK) toK
                fromK' = replicate (Z_ ::. length toK ::. All_) fromK
             in flatten $
                  zipWith
                    (\(T2 from d1) (T2 to d2) -> T2 (I2 from to) (d1 + d2))
                    toK'
                    fromK'

          (origKs, origVs) = unzip origKvs
          (newKs, newVs) = unzip viaK

          kvs1 =
            zipWithChecked3
              (\k' a mb -> T2 k' $ min a (fromMaybe inf mb))
              origKs
              origVs
              (lookupMany origKs viaK)
          kvs2 =
            zipWithChecked3
              (\k' b ma -> T2 k' $ min (fromMaybe inf ma) b)
              newKs
              newVs
              (lookupMany newKs origKvs)

          ds' = sortBy (\x y -> compare (fst x) (fst y)) (kvs1 ++ kvs2)

          nonDups =
            let ndups =
                  stencil
                    (\(l, m, _) -> l /= m)
                    (function $ const (I2 0 0))
                    (map fst ds')
             in imap (\(I1 i) b -> if i == 0 then True_ else b) ndups
       in makeCSR (afst $ compact nonDups ds')

makeCSR :: (Elt a) => Acc (Vector (DIM2, a)) -> Acc (CSRMatrix a)
makeCSR arr =
  let Z_ ::. n' ::. n'' = the $ maximum (map fst arr)
      n = max n' n'' + 1
      (k1s, kvs) = unzip $ map (\(T2 (I2 k1 k2) v) -> T2 k1 (T2 k2 v)) (sortBy (\x y -> compare (fst x) (fst y)) arr)
      k1s' = histogram (I1 n) (map I1 k1s)
   in T2 (postscanl (+) 0 k1s') kvs

-- | Lookup multiple keys in an association array.
lookupMany ::
  (Eq a, Elt b) =>
  Acc (Vector a) ->
  Acc (Vector (a, b)) ->
  Acc (Vector (Maybe b))
lookupMany ks kvs = map (`lookup` kvs) ks
  where
    lookup x ys = snd $ while condition step (T2 0 Nothing_)
      where
        condition (T2 i _) = i < (length ys)

        step (T2 i _) =
          let T2 x' y = ys !! i
           in if x' == x
                -- If found, set counter to end, stop immediately.
                -- Fewer checks required this way.
                then T2 (length ys) (Just_ y)
                else T2 (i + 1) Nothing_

histogram :: Exp DIM1 -> Acc (Vector DIM1) -> Acc (Vector Int)
histogram n ids =
  let zeros = fill n 0
      ones = fill (shape ids) 1
   in permute' (+) zeros (map Just_ $ zipChecked ids ones)
