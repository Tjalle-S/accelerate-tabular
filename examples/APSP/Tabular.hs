{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module APSP.Tabular (apsp) where

import Common
import Data.Array.Accelerate.Tabular

type Edge = Z :. Int :. Int

apsp ::
  forall rep rep'.
  (Slice rep Edge, Rep rep' (Z :. Int)) =>
  Proxy rep' ->
  Acc (Table rep Edge Float) ->
  Acc (Table rep Edge Float)
apsp _ ds = aforTab (unit n) update ds
  where
    Z_ ::. n' ::. n'' = the $ fold1All max $ keys ds
    n = max n' n''

    update ak d =
      let k = the ak

          toK = slice @rep' (Z_ ::. Keep_ ::. Slice_ k) d
          fromK = slice @rep' (Z_ ::. Slice_ k ::. Keep_) d

          viaK = cartesianWith @rep (+) toK fromK
       in fullOuterJoin @rep min inf inf d viaK
