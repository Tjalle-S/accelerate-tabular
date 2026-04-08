{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}

module Data.Array.Accelerate.Tabular.Classes.Fold (
  Fold (..)
) where

import Data.Array.Accelerate

import Data.Array.Accelerate.Tabular.Classes.Rep

-- Every type that is an instance of Rep should also be an instance of Fold.
--
-- Additionally, the combination of constraints on these type classes leads to
-- non-terminating instance search, since the instance for Fold Z Z 
-- gives a table of the same type as result.
--

-- | Class of representations that support reduction.
--
class (Rep rep key, Rep (RepFold rep key) (KeyFold rep key)) =>
  Fold rep key where

  -- | The representation for the table resulting from performing a fold.
  --
  type RepFold rep key

  -- | The key resulting for the table resulting from performing a fold.
  --
  type KeyFold rep key

  -- | Compute the metadata for the table resulting from performing a fold,
  -- and the segment descriptor for performing the fold.
  --
  foldMeta :: Acc (Meta rep key)
           -> ( Acc (Meta (RepFold rep key) (KeyFold rep key))
              , Acc (Segments Int)
              )

-- | Only for internal use.
-- 
instance Fold Z Z where

  type RepFold Z Z = Z
  type KeyFold Z Z = Z

  foldMeta _ = (emptyMeta, generate (I1 1) (const 1))
