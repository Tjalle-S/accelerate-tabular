{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE FlexibleContexts      #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Array.Accelerate.Tabular.Classes.Sugar where

import qualified Prelude as P

import Data.Array.Accelerate

import Data.Array.Accelerate.Tabular.Classes.Rep ()

import Data.Data
-- import Language.Haskell.TH hiding (Exp)
import GHC.Generics
import Data.Coerce
import Data.Array.Accelerate.Smart

-- | Specify a conversion from a surface type @key@ to an underlying type
-- is supported by generic representations (built using 'Z' and '(:.)').
--
-- @c@ is a type-level index specifying the desired conversion.
-- This is useful if there are multiple possible conversions,
-- e.g. for storing matrices in row-major or column-major order.
-- The index 'G' can be used to specify a generically derived conversion.
-- This derivation can be requested using @deriving ('Generic', 'Sugar' 'G')@,
-- using the @DeriveGeneric@ and @DeriveAnyClass@ language extensions.

class () => Sugar c key where

  type Underlying c key
  type Underlying c key = Underlying' c key Z

  type Underlying' c key prefix
  type Underlying' c key prefix = GUnderlying (Rep key) prefix

  toUnderlying   :: Proxy c -> Exp key -> Exp (Underlying c key)
  -- default toUnderlying :: (Generic key) => Proxy G -> Exp key -> Exp (Underlying G key)
  -- toUnderlying _ key = gToUnderlying 
  toSurface      :: Proxy c -> Exp (Underlying c key) -> Exp key

data Id 

instance (Eq key) => Sugar Id key where
  
  type Underlying Id key = key

  type Underlying' Id key prefix = prefix :. key

  toUnderlying _ = P.id
  toSurface    _ = P.id


data G

class (Eq (GUnderlying f Z)) => GConvert f where

  type GUnderlying f prefix

  gToUnderlying :: prefix -> f a -> GUnderlying f prefix

instance GConvert U1 where

  type GUnderlying U1 prefix = prefix

  gToUnderlying prf _ = prf

instance GConvert a => GConvert (M1 i c a) where

  type GUnderlying (M1 i c a) prefix = GUnderlying a prefix


instance (Sugar G a, Eq (Underlying' G a Z)) => GConvert (K1 i a) where

  type GUnderlying (K1 i a) prefix = Underlying' G a prefix



instance (GConvert a, GConvert b, Eq (GUnderlying b (GUnderlying a Z))) => GConvert (a :*: b) where

  type GUnderlying (a :*: b) prefix = GUnderlying b (GUnderlying a prefix)


instance Sugar G Bool where

  type Underlying' G Bool prefix = prefix :. Bool

instance Sugar G Int where

  type Underlying' G Int prefix = prefix :. Int

  toUnderlying _ x          = Z_ ::. x
  toSurface    _ (Z_ ::. x) = x

instance Sugar G Float where

  type Underlying' G Float prefix = prefix :. Float

  toUnderlying _ x          = Z_ ::. x
  toSurface    _ (Z_ ::. x) = x

instance (Eq a) => Sugar G (Maybe a) where

  type Underlying' G (Maybe a) prefix = prefix :. Maybe a

instance (Eq a, Eq b) => Sugar G (Either a b) where

  type Underlying' G (Either a b) prefix = prefix :. Either a b

-- test :: Exp (Int, Int) -> Exp (Underlying G (Int, Int))
-- test = toUnderlying (Proxy @G)

data Point = Point Int Float
  deriving (Generic, Elt, Sugar G)

pattern Point_ :: Exp Int -> Exp Float -> Exp Point
pattern Point_ { x_, y_ } = Pattern (x_, y_)
{-# COMPLETE Point_ #-}

instance Eq Point where
  p1 == p2 = x_ p1 == x_ p2 && y_ p1 == y_ p2



data Test = Test Int Point
  deriving (Generic, Elt, Sugar G)

pattern Test_ :: Exp Int -> Exp Point -> Exp Test
pattern Test_ p x = Pattern (p, x)
{-# COMPLETE Test_ #-}

instance Eq Test where
  (Test_ p1 x1) == (Test_ p2 x2) = p1 == p2 && x1 == x2

instance (Sugar G a, Sugar G b) => Sugar G (a, b)
instance (Sugar G a, Sugar G b, Sugar G c) => Sugar G (a, b, c)


-- test :: Exp (Underlying G (Int, Float)) -> Exp (Int, Float)
-- test = toSurface (Proxy @G)


-- runQ $ do
--   let
--       integralTypes :: [Name]
--       integralTypes =
--         [ ''Int
--         , ''Int8
--         , ''Int16
--         , ''Int32
--         , ''Int64
--         , ''Word
--         , ''Word8
--         , ''Word16
--         , ''Word32
--         , ''Word64
--         ]

--       floatingTypes :: [Name]
--       floatingTypes =
--         [ ''Half
--         , ''Float
--         , ''Double
--         ]

--       newtypes :: [Name]
--       newtypes =
--         [ ''CShort
--         , ''CUShort
--         , ''CInt
--         , ''CUInt
--         , ''CLong
--         , ''CULong
--         , ''CLLong
--         , ''CULLong
--         , ''CFloat
--         , ''CDouble
--         , ''CChar
--         , ''CSChar
--         , ''CUChar
--         ]

--       mkSimple :: Name -> Q [Dec]
--       mkSimple name =
--         let t = conT name
--         in
--         [d| instance Convert Prim $t where
--               type Underlying Prim $t = Z :. $t
--               toUnderlying _ = (Z_ ::. )
--           |]

--       mkTuple :: Int -> Q Dec
--       mkTuple n =
--         let
--             xs  = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
--             ts  = map varT xs
--             res = tupT ts
--             ctx = mapM (appT [t| Elt |]) ts
--         in
--         instanceD ctx [t| Elt $res |] []

--       --
--       mkNewtype :: Name -> Q [Dec]
--       mkNewtype name = do
--         r    <- reify name
--         base <- case r of
--                   TyConI (NewtypeD _ _ _ _ (NormalC _ [(_, ConT b)]) _) -> return b
--                   _                                                     -> error "unexpected case generating newtype Elt instance"
--         --
--         [d| instance Elt $(conT name) where
--               type EltR $(conT name) = $(conT base)
--               eltR = TupRsingle scalarType
--               tagsR = [TagRsingle scalarType]
--               fromElt $(conP (mkName (nameBase name)) [varP (mkName "x")]) = x
--               toElt = $(conE (mkName (nameBase name)))
--           |]
--   --
--   ss <- mapM mkSimple (integralTypes ++ floatingTypes)
--   ns <- mapM mkNewtype newtypes
--   ts <- mapM mkTuple [2..16]

--   return (concat ss ++ concat ns ++ ts)
