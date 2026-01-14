{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeOperators         #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE StandaloneDeriving    #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Array.Accelerate.Tabular (
  Table
, Rep (..)
, Select (..)
, Project (..)
, Join (..)
, Convert (..)
, Index (..)
, IsIndex (..)
, type (:.:)
) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.HashMap (HashMap, Hashable)
import qualified Data.Array.Accelerate.Data.HashMap as H

import Data.Array.Accelerate.Data.Maybe
import Data.Array.Accelerate.Data.Functor
import qualified Prelude as P
import Data.Kind

newtype Table rep key val = Table (TableR rep key val)
  deriving Generic

deriving instance Arrays (TableR rep key val) => Arrays (Table rep key val)

{-# COMPLETE Table_ #-}
pattern Table_ :: Arrays (TableR rep key val)
               => Acc (TableR rep key val)
               -> Acc (Table  rep key val)
pattern Table_ tab = Pattern tab

class (Elt key, Elt val, Arrays (TableR rep key val)) => Rep rep key val where
  type TableR rep key val

  -- unsafeIndex :: Acc (Table rep key val)-> Exp key -> Exp val
  -- index :: Acc (Table rep key val)-> Exp key -> Exp (Maybe val)

data Complete

instance (Shape key, Elt val) => Rep Complete key val where
  type TableR Complete key val = Array key val

  -- unsafeIndex (Table_ t) = (t !)
  -- index tab = Just_ . unsafeIndex tab

data Dense

instance (Shape key, Elt val) => Rep Dense key val where
  type TableR Dense key val = Array key (Maybe val)

  -- unsafeIndex tab = AM.fromJust . index tab
  -- index (Table_ t) = (t !)

data Coordinate

instance (Elt key, Elt val) => Rep Coordinate key val where
  type TableR Coordinate key val = (Scalar key, Vector (key, val))

data CSR

instance (Shape key, Elt val) => Rep CSR (key :. Int) val where
  type TableR CSR (key :. Int) val = (Scalar (key :. Int), Vector Int, Vector (key, val))

data Hash

instance (Hashable key, Elt val) => Rep Hash key val where
  type TableR Hash key val = HashMap key val

data Segmented

instance (Shape key, Elt val) => Rep Segmented key val where
  type TableR Segmented key val = (Scalar key, Segments key, Vector val)

data BitflagSegmented

-- instance 

csrToDense :: (Shape key, Elt val)
           => Acc (Table CSR   (key :. Int) val)
           -> Acc (Table Dense (key :. Int) val)
csrToDense (Table_ (T3 sh seg dat)) =
  let (idxs, vals) = unzip dat
      rowIdxs = let zeros = fill (shape vals)       (0 :: Exp Int)
                    ones  = fill (I1 $ size seg - 1) 1
                in scanl1 (+) $ permute (+) zeros (Just_ . I1 . (seg !)) ones
      resIdxs = zipWith (::.) idxs rowIdxs
      res = permute const (fill (the sh) Nothing_) (Just_ . (resIdxs !)) (map Just_ vals)
  in Table_ res

-- denseToCsr :: (Shape key, Elt val)
--            => Acc (Table Dense (key :. Int) val)
--            -> Acc (Table CSR   (key :. Int) val)
-- denseToCsr (Table_ tab) = 
--   let sh = unit (shape tab)
--       T2 dat seg = filter (isJust . snd) (indexed tab)

--       x = indexed tab
--       y = filter isJust tab
--   in  Table_ (T3 sh seg undefined)

------------------------------------------------------------

class (Rep rep key val) => Select rep key val where
  type SelectionTarget rep
  type SelectionTarget rep = rep

  select :: (Exp key -> Exp val -> Exp Bool)
         -> Acc (Table rep                   key val)
         -> Acc (Table (SelectionTarget rep) key val)

instance (Shape key, Elt val) => Select Complete key val where
  type SelectionTarget Complete = Dense

  select p (Table_ tab) = Table_ (imap f tab)
    where
      f i x = if p i x
                then Just_ x
                else Nothing_

instance (Shape key, Elt val) => Select Dense key val where
  select p (Table_ tab) = Table_ (imap f tab)
    where
      f i = maybe Nothing_ (c i)
      c i x = if p i x
                 then Just_ x
                 else Nothing_

instance (Elt key, Elt val) => Select Coordinate key val where
  select p (Table_ (T2 sh kvs)) =
    let T2 kvs' _ = filter (uncurry p) kvs
    in  Table_ (T2 sh kvs')


---------------------------------------------------------------

-- Consideration:
-- This way has an easy type class declaration.
class Rep rep key val => Project rep key val where
  project :: (Rep rep key val')
          => (Exp val -> Exp val')
          -> Acc (Table rep key val)
          -> Acc (Table rep key val')
-- Using this way instead makes things more difficult to use, but would allow specialised representations for complex values (e.g. variable-length strings; or simply for performance reasons):
--
-- class (Rep rep key val, Rep rep' key val') => Project rep rep' key val val' where
-- project :: (Exp val -> Exp val')
--         -> Acc (Table rep  key val)
--         -> Acc (Table rep' key val')

instance (Shape key, Elt val) => Project Complete key val where
  project f (Table_ tab) = Table_ (map f tab)

instance (Shape key, Elt val) => Project Dense key val where
  project f (Table_ tab) = Table_ $ map (fmap f) tab

instance (Elt key, Elt val) => Project Coordinate key val where
  project f (Table_ (T2 sh dat)) = Table_ $ T2 sh $ map (\(T2 i v) -> T2 i (f v)) dat

instance (Shape key, Elt val) => Project CSR (key :. Int) val where
  project f (Table_ (T3 sh seg dat)) = 
    let dat' = map (\(T2 i v) -> T2 i (f v)) dat
    in  Table_ (T3 sh seg dat')

instance (Hashable key, Elt val) => Project Hash key val where
  project f (Table_ tab) = Table_ (H.map f tab)

------------------------------------------------------

class (Rep rep key val, Rep rep key val') => Join rep key val val' where
  naturalJoin :: Acc (Table rep key val) -> Acc (Table rep key val') -> Acc (Table rep key (val, val'))


instance (Shape key, Elt val, Elt val') => Join Complete key val val' where
  naturalJoin (Table_ tab1) (Table_ tab2) = Table_ (zip tab1 tab2)

instance (Shape key, Elt val, Elt val') => Join Dense key val val' where
  naturalJoin (Table_ tab1) (Table_ tab2) = Table_ (zipWith f tab1 tab2)
    where
      f Nothing_  _         = Nothing_
      f _         Nothing_  = Nothing_
      f (Just_ x) (Just_ y) = Just_ (T2 x y)

-------------------------------------------------

class (Rep rep key val, Rep rep' key val) => Convert rep' rep key val where
  convert :: Acc (Table rep key val) -> Acc (Table rep' key val)

instance (Shape key, Elt val) => Convert Dense Complete key val where
  convert (Table_ tab) = Table_ $ map Just_ tab

-- test :: (Convert rep rep' DIM1 Int) => Acc (Table rep DIM1 Int) -> Acc (Table rep' DIM1 Int)
-- test = convert @Complete @Dense

---------------------------

class Rep rep key val => Index rep key val where
  index :: Acc (Table rep key val) -> Exp key -> Exp (Maybe val)
  unsafeIndex :: Acc (Table rep key val) -> Exp key -> Exp val

instance (Shape key, Elt val) => Index Complete key val where
  unsafeIndex (Table_ tab) = (tab !)
  index tab = Just_ . unsafeIndex tab

instance (Shape key, Elt val) => Index Dense key val where
  index (Table_ tab) = (tab !)
  unsafeIndex tab = fromJust . index tab

instance (Eq key, Hashable key, Elt val) => Index Hash key val where
  index (Table_ tab) = (`H.lookup` tab)
  unsafeIndex tab = fromJust . index tab

-------------------------------

class (Elt key, Shape sh) => IsIndex key sh where
  toShape :: key -> sh

instance (Shape key) => IsIndex key key where
  toShape = P.id

instance (Elt key, P.Enum key) => IsIndex key DIM1 where
  toShape = (Z :.) . P.fromEnum

instance (IsIndex key sh, Elt key', P.Enum key') => IsIndex (key, key') (sh :. Int) where
  toShape (x, y) = toShape x :. P.fromEnum y


----------------------------------------------------------------------------

-- class Arrays (TableR' rep key val) => Rep' rep key val where
--   type TableR' rep key val

-- data Dense' key
-- data Compressed' key

data r :.: r'

-- instance Rep' Z where
--   type TableR' Z val = Vector val

-- instance (Rep' rep, Shape key) => Rep' (rep :.: Dense' key) where
--   type TableR' (rep :.: Dense' key) val = (Scalar key, Table' rep val)

-- instance (Rep' rep, Shape key) => Rep' (rep :.: Compressed' key) where
--   type TableR' (rep :.: Compressed' key) val = (Table' rep val)

-- newtype Table' rep val = Table' (TableR' rep val)
--   deriving Generic

-- deriving instance Arrays (TableR' rep val) => Arrays (Table' rep val)

-- {-# COMPLETE Table'_ #-}
-- pattern Table'_ :: Arrays (TableR' rep val)
--                => Acc (TableR' rep val)
--                -> Acc (Table'  rep val)
-- pattern Table'_ tab = Pattern tab

-- class Rep' rep => Project' rep val where
--   project' :: (Elt val, Elt val')
--            => (Exp val -> Exp val')
--            -> Acc (Table' rep val)
--            -> Acc (Table' rep val')

-- instance (Elt val) => Project' Z val where
--   project' f (Table'_ tab) = Table'_ (map f tab)

-- instance (Shape key, Project' rep val) => Project' (rep :.: Dense' key) val where
--   project' f (Table'_ _) = undefined

instance (Shape key, Elt val) => Rep Z key val where
  type TableR Z key val = Vector val

instance (Shape key, Rep rep key val) => Rep (rep :.: Dense) key val where
  type TableR (rep :.: Dense) key val = (Scalar Int, Table rep key val)

data Compressed

instance (Shape key, Rep rep key val) => Rep (rep :.: Compressed) key val where
  type TableR (rep :.: Compressed) key val = (Segments DIM1, Vector Int, Table rep key val)
  
instance (Elt val, Elt val') => Project' Z Z val val' where
  project' f (Table_ tab) = Table_ (map f tab)

class (Rep rep key val, Rep rep key val') => Project' rep key val val' where
  project' :: (Exp val -> Exp val') -> Acc (Table rep key val) -> Acc (Table rep key val')
  iproject' :: (Exp key -> Exp val -> Exp val') -> Acc (Table rep key val) -> Acc (Table rep key val')



instance (Shape key, Project' rep key val val') =>
  Project' (rep :.: Dense) key val val' where
  project' f (Table_ (T2 s inner)) = Table_ $ T2 s (project' f inner)

instance (Shape key, Project' rep key val val') => Project' (rep :.: Compressed) key val val' where
  project' f (Table_ (T3 seg pos inner)) = Table_ $ T3 seg pos (project' f inner)