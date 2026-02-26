module Data.Array.Accelerate.Tabular.Util (
  emptyVector
) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Unsafe (undef)

-- | Creates an empty (length-0) vector.
emptyVector :: (Elt e) => Acc (Vector e)
emptyVector = fill (I1 0) undef
