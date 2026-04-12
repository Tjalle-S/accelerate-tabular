{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE GADTs                 #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}

module Data.Array.Accelerate.Tabular.Classes.Fold (
  Fold (..)
, FoldDescriptor (..)
, FoldDescriptor' (..)
, Group (..), Keep (..)
, FoldResult
) where

import Data.Array.Accelerate

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Rep.Snoc

import Data.Proxy

-- Every type that is an instance of Rep should also be an instance of Fold.
--
-- However, it may be necessary to keep them apart, for syntactic sugar
-- involving keys not defined using '(:.:)'.
--

class (Rep rep key) => Fold rep key where

  -- | Compute the metadata for the table resulting from performing a fold,
  -- and the segment descriptor for performing the fold.
  --
  foldMeta :: FoldDescriptor  rep key desc
           => FoldDescriptor' rep key desc
           -> Acc (Meta rep key)
           -> Acc ( Meta (FoldResult rep desc) (FoldResult key desc)
                  , Segments Int
                  )

instance Fold Z Z where

  foldMeta _ _ = let seg = fill (I1 1) 1
                 in  T2 emptyMeta seg


data Keep  = Keep
data Group = Group

class (Rep rep key, Rep (FoldResult rep desc) (FoldResult key desc)) =>
  FoldDescriptor rep key desc where

  -- | Get a value describing the structure of the descriptor.
  --
  getDescriptor :: Proxy desc -> FoldDescriptor' rep key desc
  -- Note that the use of Proxy here is not strictly required.
  -- However, having it allows the fold function to be used as e.g.
  -- `fold (Keep :.: Group)` instead of of `fold (Proxy @(Keep :.: Group))`.
  -- 

instance (Rep rep key) => FoldDescriptor rep key Keep where

  getDescriptor _ = FoldKeep

instance (FoldDescriptor rep keys desc, Rep (rep :.: r) (keys :.: key)) =>
  FoldDescriptor (rep :.: r) (keys :.: key) (desc :.: Group) where

  getDescriptor _ = FoldGroup (getDescriptor Proxy)

-- | Describes the structure of a fold descriptor in a single data type,
-- to allow pattern matching.
--
data FoldDescriptor' rep key desc where
  FoldKeep  :: FoldDescriptor' rep key  Keep
  FoldGroup :: FoldDescriptor  rep keys desc
            => FoldDescriptor' rep keys desc
            -> FoldDescriptor' (rep :.: r) (keys :.: key) (desc :.: Group)

-- | Describes the result of folding over a representation
-- with a given descriptor.
--
type family FoldResult t desc where
  FoldResult t            Keep             = t
  FoldResult (tail :.: _) (desc :.: Group) = FoldResult tail desc
