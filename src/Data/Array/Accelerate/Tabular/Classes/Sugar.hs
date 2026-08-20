{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE FlexibleContexts      #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Data.Array.Accelerate.Tabular.Classes.Sugar (
  Sugar (..)
, G
, genSugar, genSugars
) where

import qualified Prelude as P

import Data.Array.Accelerate

import Data.Array.Accelerate.Tabular.Classes.Fold
import Data.Array.Accelerate.Tabular.Classes.Key

import Data.Data
import Language.Haskell.TH hiding (Exp)
import qualified Language.Haskell.TH as TH
import Control.Monad
import Unsafe.Coerce (unsafeCoerce)

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

  type Underlying c key

  toUnderlying :: Proxy c -> Exp key -> Exp (Underlying c key)
  toSurface :: (Key (Underlying c key)) => Proxy c -> Exp (Underlying c key) -> Exp key
  toSurface proxy = P.snd . toSurface' proxy . TheKeyR . getKeyR

  toSurface' :: (Key (Underlying c key)) => Proxy c -> SomeKeyR -> (SomeKeyR, Exp key)
  toSurface' proxy (TheKeyR keyr) = (TheKeyR KeyRZ, toSurface proxy (toKey $ unsafeCoerce keyr))

  {-# MINIMAL toUnderlying, (toSurface | toSurface') #-}

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
  [d|
    instance $(getCxt tvs con) => Sugar G $fullName where

      type Underlying G $fullName = $(getUnderlying con)

      toUnderlying _ = $(genToUnderlying con)
      toSurface    _ = undefined
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
  let cKey = P.map (\t -> ConT ''Key `AppT` (ConT ''Underlying `AppT` ConT ''G `AppT` P.snd t)) ts
  let cs = cSugar P.++ cKey P.++ cElt
  return $ P.foldl AppT (TupleT $ P.length cs) cs

getUnderlying :: Con -> Q Type
getUnderlying (NormalC _ ts) =
  getUnderlying' (P.map P.snd ts)
getUnderlying (RecC    _ ts) =
  getUnderlying' (P.map (\(_, _, z) -> z) ts)
getUnderlying (InfixC a _ b) =
  getUnderlying' [P.snd a, P.snd b]
getUnderlying _ = fail "Only vanilla constructors supported"

getUnderlying' :: [Type] -> Q Type
getUnderlying' = P.foldl
  (\x -> combineType ''(++) x
       . (conT ''Underlying `appT` conT ''G `appT`)
       . return)
  (conT ''Z)

genToUnderlying :: Con -> Q TH.Exp
genToUnderlying (NormalC _ ts) = genToUnderlying' (P.map P.snd ts)
genToUnderlying (RecC    _ ts) = genToUnderlying' (P.map (\(_, _, z) -> z) ts)
genToUnderlying (InfixC a _ b) = genToUnderlying' [P.snd a, P.snd b]
genToUnderlying _              = fail "Only vanilla constructors supported"

genToUnderlying' :: [Type] -> Q TH.Exp
genToUnderlying' ts = do
  namesIn   <- replicateM (P.length ts) (newName "x")
  namesKeyR <- replicateM (P.length ts) (newName "x'")
  namesRes  <- replicateM (P.length ts) (newName "r")

  let pat = conP 'Pattern [ tupP $
        P.zipWith
          (\nm t -> sigP (varP nm) (conT ''Exp `appT` return t))
          namesIn
          ts ]

      resDec 0 = [d|$(varP $ namesRes P.!! 0) =
                      concatKey
                        Z_
                        $(varE $ namesKeyR P.!! 0)
                   |]
      resDec n = [d|$(varP $ namesRes P.!! n) =
                      concatKey
                        $(varE $ namesRes P.!! (n - 1))
                        $(varE $ namesKeyR P.!! n)
                   |]

      makeDict 0 = [|withDict' (proveKey
        Z_
        $(varE $ namesKeyR P.!! 0))|]
      makeDict n = [|withDict' (proveKey
        $(varE $ namesRes P.!! (n - 1))
        $(varE $ namesKeyR P.!! n))|]

      res n = do
        dec  <- resDec n
        let dict = makeDict n
        let res' = if n P.== (P.length ts - 1)
                     then varE $ P.last namesRes
                     else res (n + 1)
        
        LetE dec P.<$> (dict `appE` res')

      result = if P.null ts
                 then [|Z_|]
                 else res 0

  keyRDecs <- P.fmap P.concat
            $ sequence
            $ P.zipWith (\i o ->
              [d|$(varP o) = getKeyR (toUnderlying @G Proxy $(varE i))|])
              namesIn
              namesKeyR

  let body = LetE keyRDecs P.<$> result

  lam1E pat body

combineType :: Name -> Q Type -> Q Type -> Q Type
combineType nm x y = conT nm `appT` x `appT` y 

instance Sugar G Bool where

  type Underlying G Bool = Z :. Bool

  toUnderlying _ x = Z_ ::. x
  toSurface    _ (Z_ ::. x) = x

  toSurface' _ (TheKeyR (KeyRSnoc rest x)) = (TheKeyR rest, unsafeCoerce x)
  toSurface' _ _ = error "toSurface': Key does not contain Bool."

instance Sugar G Int where

  type Underlying G Int = Z :. Int

  toUnderlying _ x = Z_ ::. x
  toSurface    _ (Z_ ::. x) = x

instance Sugar G Float where

  type Underlying G Float = Z :. Float

  toUnderlying _ x = Z_ ::. x
  toSurface    _ (Z_ ::. x) = x

instance (Eq a) => Sugar G (Maybe a) where

  type Underlying G (Maybe a) = Z :. (Maybe a)

  toUnderlying _ x = Z_ ::. x
  toSurface    _ (Z_ ::. x) = x