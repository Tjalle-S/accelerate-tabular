{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- {-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import qualified Prelude

import Data.Array.Accelerate (Arrays)
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.Native hiding (Arrays)
import Data.Array.Accelerate.Tabular.Rep
import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular hiding (Assert)

-- import qualified Data.Array.Accelerate.Tabular.Prelude.Zip as Z
-- import Data.Array.Accelerate.Tabular.Util (lookupMany)
import Data.Array.Accelerate.Tabular.Prelude
import Data.Array.Accelerate.Tabular.Classes.Slice
import Data.Array.Accelerate.Tabular.Prelude.Slice
import Data.Array.Accelerate.Tabular.Classes.Fold
import Data.Array.Accelerate (inspectCompiler)
import Data.Array.Accelerate.Tabular.Prelude.Cartesian (cartesianWith)
import Data.Kind
import GHC.TypeError
import Data.Data
import Data.Array.Accelerate.Tabular.Util
-- import Data.Array.Accelerate.Tabular.Classes.Rep (Rep(orderedCreateMeta))

import Data.Array.Accelerate.Sugar.Array
import qualified Data.Array.Accelerate.Representation.Array as R
import Data.Array.Accelerate.Representation.Type (TupR(TupRpair))
import Unsafe.Coerce (unsafeCoerce)
import Control.Applicative (Const)

-- type I2 = Z :. Int :. Int
type D2 = Z :. Dense :. Dense

-- type CD = Z :. OrdCompressed :. Dense

-- type I1 = Z :. Int
-- type Sparse = Z :. OrdCompressed

-- type H = Z :. Hashed
-- type HC = Z :. Hashed :. OrdCompressed

-- type COO = Z :. NonUniqueCompressed :. UnsafeCompleteSingleton :. UnsafeCompleteSingleton

-- type I3 = Z :. Int :. Int :. Int
-- type CSF3 = Z :. OrdCompressed :. OrdCompressed :. OrdCompressed


main :: Prelude.IO ()
main = 
  let kvs = use [(Z :. 0 :. 0, 0.0), (Z :. 0 :. 1, 0.1), (Z :. 1 :. 0, 1.0), (Z :. 1 :. 1, 1.1)]
  in  Prelude.print $ run $ slice (Z_ ::. Keep_ ::. Slice_ 1) $ createTable @(Z :. Dense :. Dense) @(Z :. Int :. Int) @Float kvs

type Key = Z :. Int :. Int

inf :: Exp Float
inf = 1 / 0

apsp :: forall rep . (F rep Key) => Acc (Table rep Key Float) -> Acc (Table rep Key Float)
apsp ds = afor (A.unit n) update ds
  where
    Z_ ::. n' ::. n'' = A.the $ A.maximum $ keys ds
    n = max n' n''

    update :: Acc (A.Scalar Int) -> Acc (Table rep Key Float) -> Acc (Table rep Key Float)
    update ak d = let k = A.the ak
                      test = fold'' (Keep :. Group) (+) 0 d
                      -- test2 = map (+ 2) test--fold' (Keep :. Group) (+) 0 test
                      -- should be slice instead of filter
                      -- toK   = slice (Z_ ::. Keep_    ::. Slice_ k) d
                      -- fromK = slice (Z_ ::. Slice_ k ::. Keep_)    d
                      --added = cartesianWith @rep (+) toK fromK
                  in  undefined --fullouterjoin @rep min inf inf d added

afor :: (Arrays a) => Acc (A.Scalar Int)
                   -> (Acc (A.Scalar Int) -> Acc a -> Acc a)
                   -> Acc a
                   -> Acc a
afor n f x = asnd $ awhile
  (\(T2 i _)  -> A.zipWith (<) i n)
  (\(T2 i x') -> T2 (A.map (+ 1) i) (f i x'))
  (T2 (A.unit 0) x)


class (FKey key, Rep rep key, Rep (FRRes rep) (FKRes key)) => F rep key where

  type FRRes rep

class (Eq key, Eq (FKRes key)) => FKey key where

  type FKRes key

instance FKey Z where

  type FKRes Z = Z

instance F Z Z where

  type FRRes Z = Z

instance (FKey keys, Eq key) => FKey (keys :. key) where

  type FKRes (keys :. key) = keys

instance (F rep keys, IndexKey key) => F (rep :. Dense) (keys :. key) where

  type FRRes (rep :. Dense) = rep

-- ----------------

class (F rep key, Rep (FRRes' rep desc) (FKRes' key desc)) => FDesc rep key desc where

  type FRRes' rep desc
  type FKRes' key desc

  getDesc :: FDesc' rep key desc

instance (F rep key) => FDesc rep key Keep where

  type FRRes' rep Keep = rep
  type FKRes' key Keep = key

  getDesc = FKeep

-- instance (FDesc rep key) => FDesc rep 




data FDesc' rep key desc where
  FKeep :: FDesc' rep key Keep
  FGroup :: (FDesc rep keys desc)
         => FDesc' rep keys desc
         -> FDesc' (rep :. r) (keys :. key) (desc :. Group)


fold'' :: (FDesc rep key desc, Elt val)
       => desc
       -> (Exp val -> Exp val -> Exp val)
       -> Exp val
       -> Acc (Table rep key val)
       -> Acc (Table (FRRes' rep desc) (FKRes' key desc) val)
fold'' = undefined



-- class (Rep rep key, IsKey key, RepShape rep key ~ KeyShape key) => Fold' rep key where
  
--   type RepShape rep key :: RepR

--   -- type FoldKeepKeyRes key
--   -- type FoldKeepRepRes rep
--   -- type FoldGroupKeyRes key
--   -- type FoldGroupRepRes rep

--   type FoldRepRes' rep desc
--   type FoldKeyRes' key desc

--   -- type PeelRep rep
--   -- type PeelKey key


--   foldMeta' :: (FoldDescriptor'' desc (RepShape rep key))
--            => FoldDescriptor''' desc (RepShape rep key)
--            -> Acc (Meta rep key)
--            -> Acc ( Meta (FoldRepRes' rep desc) (FoldKeyRes' key desc)
--                   , A.Segments Int
--                   )

--   remainsRep :: (FoldDescriptor'' desc (RepShape rep key))
--              => FoldDescriptor''' desc (RepShape rep key)
--              -> Acc (Meta rep key)
--              -> Dict' (Fold' (FoldRepRes' rep desc)) (FoldKeyRes' key desc)

--   -- type IsDesc rep key desc :: Constraint

-- instance Fold' Z Z where

--   type RepShape Z Z = RepZ

--   -- type FoldKeepKeyRes Z = Z
--   -- type FoldKeepRepRes Z = Z
--   -- Should never occur.
--   -- type FoldGroupKeyRes Z = Z
--   -- type FoldGroupRepRes Z = Z

--   type FoldRepRes' Z Keep = Z
--   type FoldKeyRes' Z Keep = Z

--   -- type PeelRep Z = Z
--   -- type PeelKey Z = Z

--   foldMeta' FoldKeep' _ = T2 emptyMeta (A.fill (I1 1) 1)
--   -- foldMeta' (FoldGroup' _) _ = T2 emptyMeta (A.fill (I1 1) 1)

--   remainsRep FoldKeep' _ = Dict'


-- type IsDesc' :: RepR -> Type -> Constraint
-- type family IsDesc' sh desc where
--   IsDesc' (RepSnoc sh) (desc :. Group) = IsDesc' sh desc
--   IsDesc' sh Keep = ()


  

  -- type IsDesc Z Z Keep = ()
  -- type IsDesc Z Z _    = TypeError ('Text "")

-- instance (Fold' rep keys, IndexKey key, IsKey key) => Fold' (rep :. Dense) (keys :. key) where
  
--   type RepShape (rep :. Dense) (keys :. key) = RepSnoc (RepShape rep keys)

--   -- type FoldGroupKeyRes (keys :. key) = keys
--   -- type FoldGroupRepRes (rep :. Dense) = rep

--   type FoldRepRes' (rep :. Dense) Keep = FoldRepRes' rep Keep :. Dense
--   type FoldRepRes' (rep :. Dense) (desc :. Group) = FoldRepRes' rep desc

--   type FoldKeyRes' (keys :. key) Keep = FoldKeyRes' keys Keep :. key
--   type FoldKeyRes' (keys :. key) (desc :. Group) = FoldKeyRes' keys desc

--   remainsRep FoldKeep' (Meta_ (T2 met _)) = case remainsRep FoldKeep' met of
--     Dict' -> Dict'
--   remainsRep (FoldGroup' rest) (Meta_ (T2 met _)) = case remainsRep rest met of
--     Dict' -> Dict' 

--   -- type PeelRep (rep :. Dense) = rep
--   -- type PeelKey (keys :. key) = keys

--   foldMeta' d dmet@(Meta_ (T2 met n)) =
--     case (d, remainsRep d dmet) of
--       (FoldKeep', Dict') -> case remainsRep FoldKeep' met of
--         Dict' -> 
--           let T2 met' seg = foldMeta' FoldKeep' met
--               len      = A.sum seg
--               seg'     = A.fill (I1 $ A.the len) (A.the n)
--           in  T2 (Meta_ $ T2 met' n) seg'
--       (FoldGroup' FoldKeep', Dict') ->
--         let T2 met' seg = foldMeta' FoldKeep' met
--             len         = A.sum seg
--             seg'        = A.fill (I1 $ A.the len) (A.the n)
--         in  T2 met' seg'
--       (FoldGroup' rest, Dict') ->
--         let T2 met' seg = foldMeta' rest met
--             seg'        = A.map (* A.the n) seg
--         in  T2 met' seg'


-- data RepR = RepZ | RepSnoc RepR

-- data SRepR r where
--   SRepZ :: SRepR RepZ
--   SRepSnoc :: SRepR r -> SRepR (RepSnoc r)

-- -- type IsDesc :: RepR -> Type -> Bool
-- -- type family IsDesc r desc where
-- --   IsDesc (RepSnoc r) (desc :. Group) = IsDesc r desc
-- --   IsDesc _ Keep = True

-- -- type IsFoldDescriptor :: Type -> Type -> Constraint
-- -- type IsFoldDescriptor key desc = Assert (IsDesc (KeyShape key) desc)
-- --   ( TypeError (
-- --          ShowType desc 
-- --     :<>: Text " cannot be used as a fold descriptor for keys of type "
-- --     :<>: ShowType key)
--   -- )
-- -- TODO: maybe use typeerror library and IfStuck for better error message.

-- class FoldDescriptor'' desc dim where
  
--   -- getDescriptor' :: FoldDescriptor''' rep (FoldRepRes rep desc) key (FoldKeyRes key desc) desc
--   getDescriptor' :: FoldDescriptor''' desc dim

--   -- getDict' :: Proxy rep -> Proxy key -> Proxy desc -> Dict' (Rep (FoldRepRes rep desc)) (FoldKeyRes key desc)

-- instance FoldDescriptor'' Keep dim where

--   getDescriptor' = FoldKeep'

--   -- getDict' _ _ _ = Dict'

-- instance (FoldDescriptor'' desc dim) => FoldDescriptor'' (desc :. Group) (RepSnoc dim) where

--   getDescriptor' = FoldGroup' getDescriptor'


-- -- instance (FoldDescriptor'' rep keys desc) => FoldDescriptor'' (rep :. r) (keys :. key) (desc :. Group) where

-- --   getDescriptor' = FoldGroup' _
   


-- data FoldDescriptor''' desc dim where
--   FoldKeep' :: FoldDescriptor''' Keep dim
--   FoldGroup' :: (FoldDescriptor'' desc dim)
--              => FoldDescriptor''' desc dim
--              -> FoldDescriptor''' (desc :. Group) (RepSnoc dim)

-- fold' :: forall rep key desc a
--       .  (Fold' rep key, FoldDescriptor'' desc (KeyShape key), Elt a)
--       => desc
--       -> (Exp a -> Exp a -> Exp a)
--       -> Exp a
--       -> Acc (Table rep key a)
--       -> Acc (Table (FoldRepRes' rep desc) (FoldKeyRes' key desc) a)
-- fold' _ f e Table_ { meta_, vals_ } = 
--   let desc = getDescriptor' @desc @(KeyShape key)
--   in case remainsRep desc meta_  of
--        Dict' -> let T2 met' seg = foldMeta' desc meta_
--                 in  Table_ met' $ A.foldSeg (combineMaybe f) (Just_ e) vals_ seg

-- -- type family FoldKeyRes key desc where
-- --   FoldKeyRes key Keep = key
-- --   FoldKeyRes key Group = FoldGroupKeyRes key

-- -- type family FoldRepRes rep desc where
-- --   FoldRepRes rep Keep = rep
-- --   FoldRepRes rep Group = FoldGroupRepRes rep

-- class IsKey key where

--   type KeyShape key :: RepR

-- instance IsKey Z where

--   type KeyShape Z = RepZ

-- instance (IsKey keys) => IsKey (keys :. key) where

--   type KeyShape (keys :. key) = RepSnoc (KeyShape keys)

-- data Table' rep key val where
--   Table' :: (Rep rep key, Elt val) => Acc (Table rep key val) -> Table' rep key val

-- -- deriving instance Generic (Table' rep key val)

-- -- instance Arrays (Table' rep key val) where
-- --   type ArraysR (Table' rep key val) = ArraysR (Table rep key val)

-- --   arraysR = unsafeAssume @(Rep rep key, Elt val) $
-- --     arraysR @(Table rep key val)

-- --   fromArr (Table' tab) = fromArr tab
-- --   toArr tab = unsafeAssume @(Rep rep key, Elt val) $
-- --     Table' (toArr tab)

-- -- pattern Table'_ :: Acc (Table rep key val) -> Acc (Table' rep key val)
-- -- pattern Table'_ tab = Pattern tab

-- unsafeAssume :: forall c r . (c => r) -> r
-- unsafeAssume f = case (unsafeAxiom :: Dict'' c) of
--   Dict'' -> f

-- unsafeAxiom :: Dict'' c
-- unsafeAxiom = unsafeCoerce (Dict'' :: Dict'' ())

data Dict'' c where
  Dict'' :: c => Dict'' c
