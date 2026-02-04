{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TemplateHaskell       #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Array.Accelerate.Tabular where

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Data.Semigroup

import Prelude (Show (..))

newtype Table rep key val = Table (TableR rep key val)
  deriving Generic

deriving instance Show (TableR rep key val) => Show (Table rep key val)
deriving instance Arrays (TableR rep key val) => Arrays (Table rep key val)

{-# COMPLETE Table_ #-}
pattern Table_ :: Arrays (TableR rep key val)
               => Acc (TableR rep key val)
               -> Acc (Table  rep key val)
pattern Table_ tab = Pattern tab

class (Elt key, Elt val, Arrays (TableR rep key val)) => Rep rep key val where
  type TableR rep key val

  enumKeys :: Acc (Table rep key val) -> Acc (Vector key)

data tail :.: head = tail :.: head
  deriving (Generic, Elt)
infixl 3 :.:

instance (Show tail, Show head) => Show (tail :.: head) where
  show (t :.: h) = show t <> " :.: " <> show h

mkPattern ''(:.:)

instance (Eq tail, Eq head) => Eq (tail :.: head) where
  (t1 ::.: h1) == (t2 ::.: h2) = h1 == h2 && t1 == t2

instance (Ord tail, Ord head) => Ord (tail :.: head) where
  compare (t1 ::.: h1) (t2 ::.: h2) = compare t1 t2 <> compare h1 h2

instance Semigroup (Exp Ordering) where
  EQ_ <> y = y
  x   <> _ = x

data Dense
data Compressed

instance (Elt val) => Rep Z Z val where
  type TableR Z Z val = Vector val

  enumKeys _ = flatten (unit Z_)

instance (Rep rep key val) => Rep (rep :.: Dense) (key :.: Int) val where
  type TableR (rep :.: Dense) (key :.: Int) val =
    (Scalar Int, Table rep key val)

  enumKeys (Table_ (T2 n tab)) = expand (const $ the n) (::.:) (enumKeys tab)


instance (Rep rep key val) => Rep (rep :.: Compressed) (key :.: Int) val where
  type TableR (rep :.: Compressed) (key :.: Int) val =
    (Vector Int, Vector Int, Table rep key val)

  enumKeys (Table_ (T3 seg ks tab)) =
    expand (getCount . fst) makeKey (indexed $ enumKeys tab)
      where
        getCount (I1 i) = let next = seg !! (i + 1)
                              curr = seg !! i
                          in  next - curr

        makeKey (T2 n k) i = let i' = (seg ! n) + i
                             in  k ::.: (ks !! i')
      
class (Rep rep key val, Rep rep key val') => Project rep key val val' where
  project :: (Exp val -> Exp val')
          -> Acc (Table rep key val)
          -> Acc (Table rep key val')

instance (Elt val, Elt val') => Project Z Z val val' where
  project f (Table_ tab) = Table_ (map f tab)

instance (Project rep key val val') =>
  Project (rep :.: Dense) (key :.: Int) val val' where
  project f (Table_ (T2 s inner)) = Table_ $ T2 s (project f inner)

instance (Project rep key val val')
  => Project (rep :.: Compressed) (key :.: Int) val val' where
  project f (Table_ (T3 seg pos inner)) = Table_ $ T3 seg pos (project f inner)

--------------------------------------

class (Rep rep key val) => Index rep key val where
  unsafeToLinearIndex :: Acc (Table rep key val)
                      -> Exp key
                      -> Exp Int
  toLinearIndex :: Acc (Table rep key val)
                -> Exp key
                -> Exp (Maybe Int)

instance (Elt val) => Index Z Z val where
  unsafeToLinearIndex _ _ = 0
  toLinearIndex       _ _ = Just_ 0

instance (Index rep key val) => Index (rep :.: Dense) (key :.: Int) val where
  unsafeToLinearIndex (Table_ (T2 n tab)) (k ::.: i) =
    the n * unsafeToLinearIndex tab k + i
  toLinearIndex       (Table_ (T2 n tab)) (k ::.: i) = 
    fmap (\i' -> the n * i' + i) (toLinearIndex tab k)

testTable :: Acc (Table (Z :.: Dense :.: Dense) (Z :.: Int :.: Int) Char)
testTable =
  let dat  = use $ fromList (Z :. 26) ['a' .. 'z']
      tab0 = Table_ dat
      tab1 = Table_ (T2 (unit 2) tab0)
  in Table_ (T2 (unit 13) tab1)

type CSR   = Z :.: Dense :.: Compressed
type DIM2' = Z :.: Int   :.: Int

testTable2 :: Acc (Table CSR DIM2' Char)
testTable2 = 
  let dat  = use $ fromList (Z :. 26) ['a' .. 'z']
      tab0 = Table_ dat
      tab1 = Table_ (T2 (unit 2) tab0)
      
      seg  = use $ fromList (Z :. 3) [0, 13, 26]
      idx  = generate (Z_ ::. 26) (\(I1 i) -> i `mod` 13)
  in  Table_ (T3 seg idx tab1)

class (Rep rep key val) => Construct rep key val where
  construct :: Exp key -- ^ Extent.
            -> Acc (Vector key)
            -> Acc (Vector val)
            -> Acc (Table rep key val)

instance (Rep rep key val) => Construct (rep :.: Dense) (key :.: Int) val where
  construct (ext ::.: e) ks vs =
    let ks = undefined
    in  Table_ (T2 (unit e) undefined)

splitKeys :: (Elt k, Elt i) => Acc (Vector (k :.: i))
          -> (Acc (Vector k), Acc (Vector i))
splitKeys = unzip . map (\(k ::.: i) -> T2 k i)
