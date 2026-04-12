{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ConstraintKinds #-}


module Data.Array.Accelerate.Tabular.Classes.Fold (
  Fold (..)
, Fold' (..)
, Nat (..)
, SNat (..)
, FoldRepResult
, FoldKeyResult
) where

import Data.Array.Accelerate hiding (Assert)

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Rep.Snoc
-- import Data.Type.Ord
-- import GHC.TypeLits
import Data.Proxy
import Data.Kind (Constraint)
import GHC.TypeError

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

class (Rep rep key) => Fold' rep key where

  -- type FoldRepResult rep
  -- type FoldKeyResult key

  type Dim rep :: Nat

  type FoldRepResult rep (k :: Nat)
  type FoldKeyResult key (k :: Nat)


  foldMeta' :: (Rep (FoldRepResult rep k) (FoldKeyResult key k){-, k <= Dim rep-})
            => SNat k
            -> Acc (Meta rep key)
            -> Acc ( Meta (FoldRepResult rep k) (FoldKeyResult key k)
                   , Segments Int
                   )

instance Fold' Z Z where

  type Dim Z = Zero

  foldMeta' _ _ = let seg = fill (I1 1) 1
                  in  T2 emptyMeta seg

-- class (Rep rep key) => IsFold fold rep key n where

--   type FoldRepRes fold rep n
--   type FoldKeyRes fold key n




-- instance Fold' Z Z where

--   foldMeta' _ _ = let seg = fill (I1 1) 1
--                   in  T2 emptyMeta seg

-- type family FoldRepResult rep fold where
--   FoldRepResult Z           _            = Z
--   FoldRepResult (rep :.: r) Z            = rep :.: r
--   FoldRepResult (rep :.: _) (fold :.: _) = rep

-- type family FoldKeyResult key fold where
--   FoldKeyResult Z           _            = Z
--   FoldKeyResult (key :.: k) Z            = key :.: k
--   FoldKeyResult (key :.: _) (fold :.: _) = key

-- class (Rep rep key, Rep (FoldRepResult rep k) (FoldKeyResult key k)) => Fold' rep key (k :: Nat) where

--   -- type FoldResRep rep k
--   -- type FoldResKey key k


--   foldMeta' :: (k <= Dim rep)
--             => Proxy k
--             -> Acc ( Meta rep key )
--             -> Acc ( Meta (FoldRepResult rep k) (FoldKeyResult key k)
--                    , Segments Int
--                    )


-- instance Fold' Z Z Zero where

--   -- type FoldResRep Z 0 = Z
--   -- type FoldResKey Z 0 = Z


--   foldMeta' _ _ = let seg = fill (I1 1) 1
--                   in  T2 emptyMeta seg

-- instance (
--     Rep rep key
--   , One <= k
--   , k <= Dim rep
--   , One <= Dim rep
--   , Fold' rep key One
--   , Pred k <= Dim (FoldRepResult rep One)
--   , Rep (FoldRepResult (FoldRepResult rep One) One)
--         (FoldKeyResult (FoldKeyResult key One) One)
--   , Rep (FoldRepResult rep k) (FoldKeyResult key k))
--   => Fold' rep key k where

--   foldMeta' _ met = 
--     let T2 met'  seg  = foldMeta' (Proxy @One) met
--         T2 met'' seg' = foldMeta' (Proxy @(Pred k)) met'
--     in  T2 met'' $ foldSeg (+) 0 seg seg'

-- type family Dim rep :: Nat where
--   Dim Z           = Zero
--   Dim (rep :.: _) = Succ (Dim rep)

-- type family FoldRepResult rep (n :: Nat) where
--   FoldRepResult rep         Zero = rep
--   FoldRepResult Z           _ = Z
--   FoldRepResult (rep :.: _) One = rep
--   FoldRepResult (rep :.: _) k = FoldRepResult rep (Pred k)

-- type family FoldKeyResult key (n :: Nat) where
--   FoldKeyResult key         Zero = key
--   FoldKeyResult Z           _ = Z
--   FoldKeyResult (key :.: _) One = key
--   FoldKeyResult (key :.: _) k = FoldKeyResult key (Pred k)

type One = Succ Zero

-- | Represents the Peano natural numbers.
data Nat = Zero | Succ Nat

-- type family (n :: Nat)
type family Pred (n :: Nat) :: Nat where
  Pred Zero     = Zero
  Pred (Succ n) = n 

type family (n :: Nat) <=? (m :: Nat) :: Bool where
  Zero     <=? _        = 'True
  (Succ _) <=? Zero     = 'False
  (Succ n) <=? (Succ m) = n <=? m


type n <= m = Assert (n <=? m) (TypeError (       
    'Text "Could not satisfy "
    ':$$: 'ShowType n
    ':$$: 'Text " <= "
    ':$$: 'ShowType m))

-- | A singleton type for natural numbers.
data SNat (k :: Nat) where
  SZero :: SNat Zero
  SSucc :: SNat k -> SNat (Succ k)

-- -- | Represents an index, with exactly @n@ inhabitants.
-- data Fin n where
--   -- | Represents the first index.
--   Top ::          Fin (Succ n)
--   -- | Represents a later index.
--   Pop :: Fin n -> Fin (Succ n)

-- class KnownNat (n :: Nat) where

--   getKnownNat :: Proxy n -> SNat n

-- instance KnownNat Zero where

--   getKnownNat _ = SZero

-- instance (KnownNat n) => KnownNat (Succ n) where

--   getKnownNat _ = SSucc (getKnownNat Proxy)
