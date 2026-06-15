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
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -ddump-to-file #-}
-- {-# LANGUAGE ImpredicativeTypes #-}

module Data.Array.Accelerate.Tabular.Classes.Sugar where

import qualified Prelude as P

import Data.Array.Accelerate

import Data.Array.Accelerate.Tabular.Classes.Rep ()

import Data.Data
import Language.Haskell.TH hiding (Exp)
import qualified Language.Haskell.TH as TH
import Control.Monad

import qualified Data.Kind as K

-- import GHC.Generics
-- import Data.Coerce
-- import Data.Array.Accelerate.Smart

-- | Specify a conversion from a surface type @key@ to an underlying type
-- is supported by generic representations (built using 'Z' and '(:.)').
--
-- @c@ is a type-level index specifying the desired conversion.
-- This is useful if there are multiple possible conversions,
-- e.g. for storing matrices in row-major or column-major order.
-- The index 'G' can be used to specify a generically derived conversion.
-- This derivation can be requested using @deriving ('Generic', 'Sugar' 'G')@,
-- using the @DeriveGeneric@ and @DeriveAnyClass@ language extensions.
--
class (Elt key) => Sugar c key where

  -- type Underlying c key
  

  type Underlying' c key :: K.Type -> K.Type
  -- type Underlying' c key
  -- type Underlying' c key prefix = GUnderlying (Rep key) prefix

  toUnderlying' :: (Elt prefix) => Proxy c -> Exp prefix -> Exp key -> Exp (Underlying' c key prefix)
  toSurface' :: (Elt prefix) => Proxy c -> Exp (Underlying' c key prefix) -> Exp (prefix, key)

type Underlying c key = Underlying' c key Z

toUnderlying :: Sugar c key => Proxy c -> Exp key -> Exp (Underlying c key)
toUnderlying proxy = toUnderlying' proxy Z_

toSurface    :: Sugar c key => Proxy c -> Exp (Underlying c key) -> Exp key
toSurface proxy = snd . toSurface' proxy

data G

genSugars :: [Name] -> Q [Dec]
genSugars = fmap P.concat . mapM genSugar

genSugar :: Name -> Q [Dec]
genSugar name = do
  info <- reify name
  case info of
    TyConI dec -> genSugarDec dec
    _          -> fail "Expected type or newtype"

genSugarDec :: Dec -> Q [Dec]
genSugarDec (NewtypeD _ name tv _ con   _) = genSugarDataD name tv con
genSugarDec (DataD    _ name tv _ [con] _) = genSugarDataD name tv con
genSugarDec (DataD    _ _    _  _ _     _) = fail "Sum types not supported"
genSugarDec _                              = fail "Expected newtype or data declaration"

genSugarDataD :: Name -> [TyVarBndr a] -> Con -> Q [Dec]
genSugarDataD name tvs con = do
  let fullName = P.foldl appTyVar (conT name) tvs
      prefixT  = varT (mkName "prefix") :: Q Type
  [d|
    instance $(getCxt tvs con) => Sugar G $fullName where

      type Underlying' G $fullName $prefixT = $(getUnderlying con prefixT)

      toUnderlying' _ _ _ = undefined
      toSurface'    _ _ = undefined
    |]

appTyVar :: Q Type -> TyVarBndr a -> Q Type
appTyVar t (PlainTV  n _)   = appT t (varT n)
appTyVar t (KindedTV n _ _) = appT t (varT n)

varTFromBndr :: TyVarBndr a -> Type
varTFromBndr (PlainTV  n _)   = VarT n
varTFromBndr (KindedTV n _ _) = VarT n


getCxt :: [TyVarBndr a] -> Con -> Q Type
getCxt tvs (NormalC _ ts) = getCxt' tvs ts
getCxt tvs (RecC    _ ts) = getCxt' tvs $ P.map (\(_, y, z) -> (y, z)) ts
getCxt tvs (InfixC a _ b) = getCxt' tvs [a, b]
getCxt _   _              = fail "Only vanilla constructors supported"

getCxt' :: [TyVarBndr a] -> [BangType] -> Q Type
getCxt' tvs ts = do
  let sugar = ''Sugar
  Just g <- lookupTypeName "G"
  Just elt <- lookupTypeName "Elt"
  let sugarG = AppT (ConT sugar) (ConT g)
  let cSugar = P.map (AppT sugarG . P.snd) ts
  let cElt   = P.map (AppT (ConT elt) . varTFromBndr) tvs
  let cs = cSugar P.++ cElt
  return $ P.foldl AppT (TupleT $ P.length cs) cs

getUnderlying :: Con -> Q Type -> Q Type
getUnderlying (NormalC _ ts) =
  getUnderlying' (P.map P.snd ts)
getUnderlying (RecC    _ ts) =
  getUnderlying' (P.map (\(_, _, z) -> z) ts)
getUnderlying (InfixC a _ b) =
  getUnderlying' [P.snd a, P.snd b]
getUnderlying _ = const $ fail "Only vanilla constructors supported"

getUnderlying' :: [Type] -> Q Type -> Q Type
getUnderlying' []       prefix = prefix
getUnderlying' (t : ts) prefix =
  let prefix' = [t|$(conT ''Underlying') $(conT ''G) $(return t) $prefix|]
  in getUnderlying' ts prefix'

genToUnderlying :: Con -> Q TH.Exp
genToUnderlying = undefined

genToUnderlying' :: Int -> Q TH.Exp
genToUnderlying' n = undefined


combineType :: Q Type -> Q Type -> Q Type
combineType x y = appT (appT (conT ''(:.)) x) y

-- Underlying (a, Bool, Float)
-- Z :. a :. Bool :. Float

-- class (Eq (GUnderlying f Z)) => GConvert f where

--   type GUnderlying f prefix

--   gToUnderlying :: prefix -> f a -> GUnderlying f prefix

-- instance GConvert U1 where

--   type GUnderlying U1 prefix = prefix

--   gToUnderlying prefix _ = prefix

-- instance GConvert a => GConvert (M1 i c a) where

--   type GUnderlying (M1 i c a) prefix = GUnderlying a prefix

--   gToUnderlying prefix (M1 a)  = gToUnderlying prefix a


-- instance (Sugar G a, Eq (Underlying' G a Z)) => GConvert (K1 i a) where

--   type GUnderlying (K1 i a) prefix = Underlying' G a prefix



-- instance (GConvert a, GConvert b, Eq (GUnderlying b (GUnderlying a Z))) => GConvert (a :*: b) where

--   type GUnderlying (a :*: b) prefix = GUnderlying b (GUnderlying a prefix)
-- type Flip f a b = f b a

newtype a :.: b = F (b :. a)
  deriving (Generic, Elt)

mkPattern ''(:.:)

pattern (::.:) :: (Elt a, Elt b)
               => Exp a
               -> Exp b
               -> Exp (a :.: b)
pattern (::.:) x y = F_ (y ::. x)
{-# COMPLETE (::.:) #-}

-- newDeclarationGroup

instance Sugar G Bool where

  type Underlying' G Bool = (:.:) Bool

  toUnderlying' _ prefix          x = x ::.: prefix
  toSurface'    _ (x ::.: prefix) = T2 prefix x

-- instance Sugar G Int where

--   type Underlying' G Int = (:.:) Int

--   toUnderlying' _ prefix              x = x ::.: prefix
--   toSurface'    _ (prefix ::. x)        = T2 prefix x

-- instance Sugar G Float where

--   type Underlying' G Float = (:.:) Float

--   toUnderlying' _ x              prefix = prefix ::. x
--   toSurface'    _ (prefix ::. x)        = T2 prefix x

-- instance (Eq a) => Sugar G (Maybe a) where

--   type Underlying' G (Maybe a) = (:.:) (Maybe a)

--   toUnderlying' _ x              prefix = prefix ::. x
--   toSurface'    _ (prefix ::. x)        = T2 prefix x

-- instance (Eq a, Eq b) => Sugar G (Either a b) where

--   type Underlying' G (Either a b) = (:.:) (Either a b)

--   toUnderlying' _ x              prefix = prefix ::. x
--   toSurface'    _ (prefix ::. x)        = T2 prefix x

-- data Point = Point Int Float
--   deriving (Generic, Elt, Sugar G)

-- pattern Point_ :: Exp Int -> Exp Float -> Exp Point
-- pattern Point_ { x_, y_ } = Pattern (x_, y_)
-- {-# COMPLETE Point_ #-}

-- instance Eq Point where
--   p1 == p2 = x_ p1 == x_ p2 && y_ p1 == y_ p2



-- data Test = Test Int Point
--   deriving (Generic, Elt, Sugar G)

-- pattern Test_ :: Exp Int -> Exp Point -> Exp Test
-- pattern Test_ p x = Pattern (p, x)
-- {-# COMPLETE Test_ #-}

-- instance Eq Test where
--   (Test_ p1 x1) == (Test_ p2 x2) = p1 == p2 && x1 == x2

-- instance (Sugar G a, Sugar G b) => Sugar G (a, b)
-- instance (Sugar G a, Sugar G b, Sugar G c) => Sugar G (a, b, c)


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
