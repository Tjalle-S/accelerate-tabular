{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ConstraintKinds #-}

{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -ddump-to-file #-}

module Data.Array.Accelerate.Tabular.Rep.Sugar (
  SugarR
, toUnderlyingMeta
, toSurfaceMeta
, Test
) where

import Data.Array.Accelerate hiding (Slice)

import Data.Data

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Classes.Slice
import Data.Array.Accelerate.Tabular.Classes.Sugar

import Data.Coerce
import Lens.Micro
import Data.Array.Accelerate.Data.Lens ()

-- | A representation with a layer of syntacic sugar over the key.
--
-- Concretely, for any @key@ for which a conversion @c@ exists
-- (i.e. an instance @'Convert' c key@), supported by a representation @rep@
-- (@'Rep' rep ('Underlying' c key)@), @'Conv' c rep@ is the same as @rep@,
-- but supporting the chosen @key@.
--
-- There is no performance penalty for using 'SugarR', if the specified
-- conversion functions perform no work aside from potentially reordering.
--
data SugarR c rep

instance (Sugar c key, Rep rep (Underlying c key), Eq key)
  => Rep (SugarR c rep) key where

  type MetaR (SugarR c rep) key = MetaR rep (Underlying c key)

  type Ordered   (SugarR c rep) = Ordered   rep
  type FastIndex (SugarR c rep) = FastIndex rep

  getIndexConstraint _ _ =
    case getIndexConstraint (Proxy @rep) (Proxy @(Underlying c key)) of
      Dict   -> Dict
      NoDict -> NoDict

  emptyMeta = toSurfaceMeta emptyMeta

  createMeta o = over _1 toSurfaceMeta
               . createMeta @rep o
               . map (toUnderlying $ Proxy @c)

  enumKeys = map (toSurface $ Proxy @c) . enumKeys @rep . toUnderlyingMeta
  

instance (Sugar c key, Index rep (Underlying c key), Eq key)
  => Index (SugarR c rep) key where

  toLinearIndex   met = toLinearIndex (toUnderlyingMeta met)
                      . toUnderlying (Proxy @c)
  toLinearIndices met = toLinearIndices (toUnderlyingMeta met)
                      . map (toUnderlying $ Proxy @c)

  unsafeToLinearIndex   met = unsafeToLinearIndex (toUnderlyingMeta met)
                            . toUnderlying (Proxy @c)
  unsafeToLinearIndices met = unsafeToLinearIndices (toUnderlyingMeta met)
                            . map (toUnderlying $ Proxy @c)

instance (Sugar c key, Rep rep (Underlying c key), Eq key)
  => Slice (SugarR c rep) key

-- | Safely coerce table metadata for a surface key type to the one
-- underlying it.
--
toUnderlyingMeta :: (Rep (SugarR c rep) key)
                 => Acc (Meta (SugarR c rep) key)
                 -> Acc (Meta rep (Underlying c key))
toUnderlyingMeta (Meta_ met) = Meta_ (coerce met)

-- | Safely coerce table metadata to using a surface key type.
--
toSurfaceMeta :: (Rep (SugarR c rep) key)
              => Acc (Meta rep (Underlying c key))
              -> Acc (Meta (SugarR c rep) key)
toSurfaceMeta (Meta_ met) = Meta_ (coerce met)


data Test a b = Test (Maybe a) b Bool
  deriving (Generic, Elt)

genSugar ''Test


-- data Test2 = Test2 (Maybe Int) Bool
--   deriving (Generic, Elt)

-- mkPattern ''Test2
