module Data.Array.Accelerate.Tabular.Util (
  emptyVector
, fst3, snd3, thd3
) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Unsafe (undef)

-- | Creates an empty (length-0) vector.
emptyVector :: (Elt e) => Acc (Vector e)
emptyVector = fill (I1 0) undef

fst3 :: (Elt a, Elt b, Elt c) => Exp (a, b, c) -> Exp a
fst3 (T3 x _ _) = x

snd3 :: (Elt a, Elt b, Elt c) => Exp (a, b, c) -> Exp b
snd3 (T3 _ y _) = y

thd3 :: (Elt a, Elt b, Elt c) => Exp (a, b, c) -> Exp c
thd3 (T3 _ _ z) = z
