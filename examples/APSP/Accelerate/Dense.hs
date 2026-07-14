{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}

module APSP.Accelerate.Dense (apsp) where

import Common
import Data.Array.Accelerate

apsp :: Acc (Matrix Float) -> Acc (Matrix Float)
apsp ds = aforArr (unit n) update ds
  where
    Z_ ::. n' ::. n'' = shape ds
    n = max n' n''

    update ak d = 
      let k = the ak
      
          toK   = slice d (Z_ ::. All_ ::. k)
          fromK = slice d (Z_ ::. k    ::. All_)

          toK'   = replicate (Z_ ::. All_ ::. k)    toK
          fromK' = replicate (Z_ ::. k    ::. All_) fromK

          viaK = zipWith (+) toK' fromK'
      in  zipWith min viaK ds
