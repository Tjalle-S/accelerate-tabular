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
  forall rep.
  (Slice rep Edge) =>
  Acc (Table rep Edge Float) ->
  Acc (Table rep Edge Float)
apsp ds = aforTab (unit n) update ds
  where
    Z_ ::. n' ::. n'' = the $ fold1All max $ keys ds
    n = max n' n''

    update ak d =
      let k = the ak

          toK = slice @(Z :. Dense) (Z_ ::. Keep_ ::. Slice_ k) d
          fromK = slice @(Z :. Dense) (Z_ ::. Slice_ k ::. Keep_) d

          viaK = cartesianWith @rep (+) toK fromK
       in fullOuterJoin @rep min inf inf d viaK
