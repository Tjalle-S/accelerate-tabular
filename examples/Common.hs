{-# LANGUAGE NoImplicitPrelude #-}

module Common (
  module Common
) where

import Data.Array.Accelerate
import qualified Data.Array.Accelerate.Tabular as T

inf :: Exp Float
inf = 1 / 0

aforTab :: (Arrays a) => Acc (T.Scalar Int)
                      -> (Acc (T.Scalar Int) -> Acc a -> Acc a)
                      -> Acc a
                      -> Acc a
aforTab n f x = asnd $ T.awhile
  (\(T2 i _)  -> T.cartesianWith (<) i n)
  (\(T2 i x') -> T2 (T.map (+ 1) i) (f i x'))
  (T2 (T.unit 0) x)

aforArr :: (Arrays a) => Acc (Scalar Int)
                      -> (Acc (Scalar Int) -> Acc a -> Acc a)
                      -> Acc a
                      -> Acc a
aforArr n f x = asnd $ awhile
  (\(T2 i _)  -> zipWith (<) i n)
  (\(T2 i x') -> T2 (map (+ 1) i) (f i x'))
  (T2 (unit 0) x)
