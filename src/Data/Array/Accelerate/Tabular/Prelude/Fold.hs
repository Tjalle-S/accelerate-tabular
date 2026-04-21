{-# LANGUAGE NoImplicitPrelude    #-}

{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE ConstraintKinds      #-}  

{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DataKinds            #-}

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Array.Accelerate.Tabular.Prelude.Fold (
  fold, fold1
, foldAll, fold1All

, reindex

, inner, inner1, inner2, inner3

, module Fold
) where

import qualified Prelude as P

import Data.Array.Accelerate hiding (Scalar, fold, fold1, foldAll, fold1All)
import qualified Data.Array.Accelerate as A

import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Data.Maybe

import Data.Array.Accelerate.Data.Lens ()

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Rep
import Data.Array.Accelerate.Tabular.Classes.Fold as Fold
import Data.Array.Accelerate.Tabular.Prelude.Table

import GHC.TypeLits
import Control.Applicative (pure)

import Lens.Micro

-- | Reduction of a table of arbitrary dimensionality.
-- The first argument needs to be function that is both associative /and/ commutative.
--
-- Folding can only be performed over 1 or more /innermost/ dimensions.
-- When folding over different dimensions, use 'reindex' first to reorder the dimensions.
--
fold :: (NotScalar rep, Fold rep key, Elt val, FoldDescriptor rep key desc)
     => desc
     -> (Exp val -> Exp val -> Exp val)
     -> Exp val
     -> Acc (Table rep key val)
     -> Acc (Table (FoldResult rep desc) (FoldResult key desc) val)
fold d f e Table_ { meta_, vals_ } = 
  let T2 met' seg = foldMeta (getDescriptor $ pure d) meta_
  in  Table_ met' $ foldSeg (combineMaybe f) (Just_ e) vals_ seg

-- | Variant of 'fold' that requires each segment being folded over to be
-- non-empty, and does not need a default value.
--
fold1 :: (NotScalar rep, Fold rep key, Elt val, FoldDescriptor rep key desc)
      => desc
      -> (Exp val -> Exp val -> Exp val)
      -> Acc (Table rep key val)
      -> Acc (Table (FoldResult rep desc) (FoldResult key desc) val)
fold1 d f Table_ { meta_, vals_ } = 
  let T2 met' seg = foldMeta (getDescriptor $ pure d) meta_
  in  Table_ met' $ fold1Seg (combineMaybe f) vals_ seg

-- | Reduction of a table of arbitrary dimensionality to a single scalar value.
-- The first argument needs to be function that is both associative /and/ commutative.
--
foldAll :: (NotScalar rep, Rep rep key, Elt val)
        => (Exp val -> Exp val -> Exp val)
        -> Exp val
        -> Acc (Table rep key val)
        -> Acc (Scalar val)
foldAll f e Table_ { vals_ } = 
  let res = A.fold (combineMaybe f) (Just_ e) vals_
  in  Table_ emptyMeta (flatten res)

-- | Variant of 'foldAll' that requires the table to be non-empty
-- and does not need a default value.
--
fold1All :: (NotScalar rep, Rep rep key, Elt val)
        => (Exp val -> Exp val -> Exp val)
        -> Acc (Table rep key val)
        -> Acc (Scalar val)
fold1All f Table_ { vals_ } = 
  let res = A.fold1 (combineMaybe f) vals_
  in  Table_ emptyMeta (flatten res)

-- | Reindex a table by providing a mapping of keys.
--
-- Can be used to ensure the dimensions being folded over are innermost.
--
-- If the key mapping is not injective, values belonging to duplicate keys are
-- combined using the specified combination function.
--
-- If the key mapping is injective, use 'reindexUnique' instead,
-- or pass 'const' as combination function.
--
-- When changing only the underlying representation, use 'reindex'' instead,
-- or pass 'const' and 'P.id'.
--
reindex :: (NotScalarReindex rep', Rep rep key, Rep rep' key', Elt val)
        => (Exp val -> Exp val -> Exp val)
        -> (Exp key -> Exp key')
        -> Acc (Table rep  key val)
        -> Acc (Table rep' key' val)
reindex combv mapk Table_ { meta_, vals_ } =
  let (ks', vs') = unzip 
                 $ afst
                 $ justs
                 $ A.map (fmap $ over _1 mapk)
                 $ zipWithChecked
                     (\k v -> maybe Nothing_ (Just_ . T2 k) v)
                     (enumKeys meta_)
                     vals_
      T3 met perm n = createMeta ks'
      vals' = permute'
        (combineMaybe combv)
        (fill (I1 $ the n) Nothing_)
        (zipWithChecked
          (\i v -> Just_ $ T2 i (Just_ v))
          perm
          vs'
        )
  in  Table_ met vals'

-- | Like 'reindex', but assumes the key mapping is injective.
--
reindexUnique :: (NotScalarReindex rep', Rep rep key, Rep rep' key', Elt val)
              => (Exp key -> Exp key')
              -> Acc (Table rep  key val)
              -> Acc (Table rep' key' val)
reindexUnique = reindex const

-- | Change the underlying representation of a table.
--
reindex' :: (NotScalarReindex rep', Rep rep key, Rep rep' key, Elt val)
         => Acc (Table rep  key val)
         -> Acc (Table rep' key val)
reindex' = reindex const P.id

-- | Lift a combination function to a combination function on 'Maybe's.
--
combineMaybe :: Elt a
             => (Exp a -> Exp a -> Exp a)
             -> Exp (Maybe a)
             -> Exp (Maybe a)
             -> Exp (Maybe a)
combineMaybe f mx my = T2 mx my & match \case
  T2 Nothing_  Nothing_  -> Nothing_
  T2 (Just_ x) Nothing_  -> Just_ x
  T2 Nothing_  (Just_ y) -> Just_ y
  T2 (Just_ x) (Just_ y) -> Just_ (f x y)

-- | Reindexing cannot be used to create a scalar table.
--
type NotScalarReindex rep = NotScalar' (
       Text "Cannot reindex to a scalar Table (Table Z Z val)."
  :$$: Text "If you intended to reduce to a scalar, use Data.Array.Accelerate.Tabular.foldAll instead.") rep


-- Ideally, this would be a warning rather than an error, since
-- it will work correctly in all cases, giving the same results as id.
-- However, since custom warnings do not exist, it is an error instead.

-- | Folds should not be executed on scalar tables.
-- 
type NotScalar rep = NotScalar' (
  Text "Folds on scalar tables (Table Z Z val) perform no work and should be omitted.") rep

-- Common fold descriptors
-- -----------------------

-- | Reduce the innermost dimension of a table.
--
inner, inner1 :: Keep :.: Group
inner  = Keep :.: Group
inner1 = inner

-- | Reduce the 2 innermost dimensions of a table.
--
inner2 :: Keep :.: Group :.: Group
inner2 = inner1 :.: Group

-- | Reduce the 3 innermost dimensions of a table.
--
inner3 :: Keep :.: Group :.: Group :.: Group
inner3 = inner2 :.: Group
