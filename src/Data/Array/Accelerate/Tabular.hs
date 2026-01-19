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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Data.Array.Accelerate.Tabular where

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.HashMap (HashMap, Hashable)
import qualified Data.Array.Accelerate.Data.HashMap as H

import Data.Array.Accelerate.Data.Maybe
import Data.Array.Accelerate.Data.Functor
import qualified Prelude as P
import Data.Kind
import Data.Data (Proxy)

newtype Table rep key val = Table (TableR rep key val)
  deriving Generic

deriving instance Show (TableR rep key val) => Show (Table rep key val)
deriving instance Arrays (TableR rep key val) => Arrays (Table rep key val)


{-# COMPLETE Table_ #-}
pattern Table_ :: Arrays (TableR rep key val)
               => Acc (TableR rep key val)
               -> Acc (Table  rep key val)
pattern Table_ tab = Pattern tab


type family xs ++ ys where
  (x :.: xs) ++ ys = x :.: (xs ++ ys)
  x          ++ ys = x :.: ys

class (Elt key, Elt val, Arrays (TableR rep key val), Elt (IndexR rep key)) => Rep rep key val where
  type TableR rep key val
  type IndexR rep key

  indexed' :: (Elt k) 
           => Acc (Table rep key val)
           -> Acc (Vector k)
           -> Acc (Vector (k ++ key))

  indexed :: Acc (Table rep key val)
          -> Acc (Table rep key (key, val))

  -- unsafeIndex :: Acc (Table rep key val)-> Exp key -> Exp val
  -- index :: Acc (Table rep key val)-> Exp key -> Exp (Maybe val)

-- data Complete

-- instance (Shape key, Elt val) => Rep Complete key val where
--   type TableR Complete key val = Array key val

--   -- unsafeIndex (Table_ t) = (t !)
--   -- index tab = Just_ . unsafeIndex tab

-- data Dense

-- instance (Shape key, Elt val) => Rep Dense key val where
--   type TableR Dense key val = Array key (Maybe val)

--   -- unsafeIndex tab = AM.fromJust . index tab
--   -- index (Table_ t) = (t !)

-- data Coordinate

-- instance (Elt key, Elt val) => Rep Coordinate key val where
--   type TableR Coordinate key val = (Scalar key, Vector (key, val))

-- data CSR

-- instance (Shape key, Elt val) => Rep CSR (key :. Int) val where
--   type TableR CSR (key :. Int) val = (Scalar (key :. Int), Vector Int, Vector (key, val))

-- data Hash

-- instance (Hashable key, Elt val) => Rep Hash key val where
--   type TableR Hash key val = HashMap key val

-- data Segmented

-- instance (Shape key, Elt val) => Rep Segmented key val where
--   type TableR Segmented key val = (Scalar key, Segments key, Vector val)

-- data BitflagSegmented

-- -- instance 

-- csrToDense :: (Shape key, Elt val)
--            => Acc (Table CSR   (key :. Int) val)
--            -> Acc (Table Dense (key :. Int) val)
-- csrToDense (Table_ (T3 sh seg dat)) =
--   let (idxs, vals) = unzip dat
--       rowIdxs = let zeros = fill (shape vals)       (0 :: Exp Int)
--                     ones  = fill (I1 $ size seg - 1) 1
--                 in scanl1 (+) $ permute (+) zeros (Just_ . I1 . (seg !)) ones
--       resIdxs = zipWith (::.) idxs rowIdxs
--       res = permute const (fill (the sh) Nothing_) (Just_ . (resIdxs !)) (map Just_ vals)
--   in Table_ res

-- -- denseToCsr :: (Shape key, Elt val)
-- --            => Acc (Table Dense (key :. Int) val)
-- --            -> Acc (Table CSR   (key :. Int) val)
-- -- denseToCsr (Table_ tab) = 
-- --   let sh = unit (shape tab)
-- --       T2 dat seg = filter (isJust . snd) (indexed tab)

-- --       x = indexed tab
-- --       y = filter isJust tab
-- --   in  Table_ (T3 sh seg undefined)

-- ------------------------------------------------------------

-- class (Rep rep key val) => Select rep key val where
--   type SelectionTarget rep
--   type SelectionTarget rep = rep

--   select :: (Exp key -> Exp val -> Exp Bool)
--          -> Acc (Table rep                   key val)
--          -> Acc (Table (SelectionTarget rep) key val)

-- instance (Shape key, Elt val) => Select Complete key val where
--   type SelectionTarget Complete = Dense

--   select p (Table_ tab) = Table_ (imap f tab)
--     where
--       f i x = if p i x
--                 then Just_ x
--                 else Nothing_

-- instance (Shape key, Elt val) => Select Dense key val where
--   select p (Table_ tab) = Table_ (imap f tab)
--     where
--       f i = maybe Nothing_ (c i)
--       c i x = if p i x
--                  then Just_ x
--                  else Nothing_

-- instance (Elt key, Elt val) => Select Coordinate key val where
--   select p (Table_ (T2 sh kvs)) =
--     let T2 kvs' _ = filter (uncurry p) kvs
--     in  Table_ (T2 sh kvs')


-- ---------------------------------------------------------------

-- -- Consideration:
-- -- This way has an easy type class declaration.
-- class Rep rep key val => Project rep key val where
--   project :: (Rep rep key val')
--           => (Exp val -> Exp val')
--           -> Acc (Table rep key val)
--           -> Acc (Table rep key val')
-- -- Using this way instead makes things more difficult to use, but would allow specialised representations for complex values (e.g. variable-length strings; or simply for performance reasons):
-- --
-- -- class (Rep rep key val, Rep rep' key val') => Project rep rep' key val val' where
-- -- project :: (Exp val -> Exp val')
-- --         -> Acc (Table rep  key val)
-- --         -> Acc (Table rep' key val')

-- instance (Shape key, Elt val) => Project Complete key val where
--   project f (Table_ tab) = Table_ (map f tab)

-- instance (Shape key, Elt val) => Project Dense key val where
--   project f (Table_ tab) = Table_ $ map (fmap f) tab

-- instance (Elt key, Elt val) => Project Coordinate key val where
--   project f (Table_ (T2 sh dat)) = Table_ $ T2 sh $ map (\(T2 i v) -> T2 i (f v)) dat

-- instance (Shape key, Elt val) => Project CSR (key :. Int) val where
--   project f (Table_ (T3 sh seg dat)) =
--     let dat' = map (\(T2 i v) -> T2 i (f v)) dat
--     in  Table_ (T3 sh seg dat')

-- instance (Hashable key, Elt val) => Project Hash key val where
--   project f (Table_ tab) = Table_ (H.map f tab)

-- ------------------------------------------------------

-- class (Rep rep key val, Rep rep key val') => Join rep key val val' where
--   naturalJoin :: Acc (Table rep key val) -> Acc (Table rep key val') -> Acc (Table rep key (val, val'))


-- instance (Shape key, Elt val, Elt val') => Join Complete key val val' where
--   naturalJoin (Table_ tab1) (Table_ tab2) = Table_ (zip tab1 tab2)

-- instance (Shape key, Elt val, Elt val') => Join Dense key val val' where
--   naturalJoin (Table_ tab1) (Table_ tab2) = Table_ (zipWith f tab1 tab2)
--     where
--       f Nothing_  _         = Nothing_
--       f _         Nothing_  = Nothing_
--       f (Just_ x) (Just_ y) = Just_ (T2 x y)

-- -------------------------------------------------

-- class (Rep rep key val, Rep rep' key val) => Convert rep' rep key val where
--   convert :: Acc (Table rep key val) -> Acc (Table rep' key val)

-- instance (Shape key, Elt val) => Convert Dense Complete key val where
--   convert (Table_ tab) = Table_ $ map Just_ tab

-- -- test :: (Convert rep rep' DIM1 Int) => Acc (Table rep DIM1 Int) -> Acc (Table rep' DIM1 Int)
-- -- test = convert @Complete @Dense

-- ---------------------------

-- class Rep rep key val => Index rep key val where
--   index :: Acc (Table rep key val) -> Exp key -> Exp (Maybe val)
--   unsafeIndex :: Acc (Table rep key val) -> Exp key -> Exp val

-- instance (Shape key, Elt val) => Index Complete key val where
--   unsafeIndex (Table_ tab) = (tab !)
--   index tab = Just_ . unsafeIndex tab

-- instance (Shape key, Elt val) => Index Dense key val where
--   index (Table_ tab) = (tab !)
--   unsafeIndex tab = fromJust . index tab

-- instance (Eq key, Hashable key, Elt val) => Index Hash key val where
--   index (Table_ tab) = (`H.lookup` tab)
--   unsafeIndex tab = fromJust . index tab

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

data head :.: tail = head :.: tail
  deriving (Generic, Elt)
infixr 3 :.:

mkPattern ''(:.:)

data Dense
data Compressed

-- instance Rep' Z where
--   type TableR' Z val = Vector val

-- instance (Rep' rep, Shape key) => Rep' (Dense :.: rep' key) where
--   type TableR' (Dense :.: rep' key) val = (Scalar key, Table' rep val)

-- instance (Rep' rep, Shape key) => Rep' (Compressed :.: rep' key) where
--   type TableR' (Compressed :.: rep' key) val = (Table' rep val)

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

-- instance (Shape key, Project' rep val) => Project' (Dense :.: rep' key) val where
--   project' f (Table'_ _) = undefined

-- data HList l where
--   HNil :: 


instance (Elt val) => Rep Z Z val where
  type TableR Z Z val = Vector val
  type IndexR Z Z = Z

  -- indexed' _ = map (::.: Z_)
  indexed (Table_ tab) = Table_ $ map (T2 Z_) tab

instance (Rep rep key val) => Rep (Dense :.: rep) (Int :.: key) val where
  type TableR (Dense :.: rep) (Int :.: key) val = (Scalar Int, Table rep key val)
  type IndexR (Dense :.: rep) (Int :.: key) = Int

  -- indexed' (Table_ (T2 n tab)) = expand (const $ the n) (::.:)
      -- Then: indexed' tab . expand (const $ the n) (::.:)
      -- But how to make this type-check.
  -- indexed (Table_ (T2 n tab)) = let x = expand (const $ the n) undefined tab
  --                               in  Table_ (T2 n $ Table_ x)

instance (Rep rep key val) => Rep (Compressed :.: rep) (Int :.: key) val where
  type TableR (Compressed :.: rep) (Int :.: key) val = (Vector Int, Vector Int, Table rep key val)
  type IndexR (Compressed :.: rep) (Int :.: key) = Int

class (Rep rep key val, Rep rep key val') => Project' rep key val val' where
  project' :: (Exp val -> Exp val') -> Acc (Table rep key val) -> Acc (Table rep key val')

instance (Elt val, Elt val') => Project' Z Z val val' where
  project' f (Table_ tab) = Table_ (map f tab)

instance (Project' rep key val val') =>
  Project' (Dense :.: rep) (Int :.: key) val val' where
  project' f (Table_ (T2 s inner)) = Table_ $ T2 s (project' f inner)

instance (Project' rep key val val')
  => Project' (Compressed :.: rep) (Int :.: key) val val' where
  project' f (Table_ (T3 seg pos inner)) = Table_ $ T3 seg pos (project' f inner)

--------------------------------------

class (Rep rep key val) => Index' rep key val where
  unsafeIndex' :: Exp Int                 -- ^ Previous real position.
               -> Acc (Table rep key val) -- ^ The table to index.
               -> Exp key                 -- ^ The key to find
               -> Exp val

instance (Elt val) => Index' Z Z val where
  unsafeIndex' p (Table_ tab) _ = tab !! p

instance (Index' rep key val) => Index' (Dense :.: rep) (Int :.: key) val where
  unsafeIndex' p (Table_ (T2 n tab)) (i ::.: k) =
    unsafeIndex' (p * the n + i) tab k

testTable :: Acc (Table (Dense :.: Dense :.: Z) (Int :.: Int :.: Z) Char)
testTable =
  let dat  = use $ fromList (Z :. 26) ['a' .. 'z']
      tab0 = Table_ dat
      tab1 = Table_ (T2 (unit 13) tab0)
      tab2 = Table_ (T2 (unit 2)  tab1)
  in tab2
