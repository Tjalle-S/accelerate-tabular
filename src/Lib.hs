{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Prelude (IO, putStrLn, type (~), Functor)

import Data.Array.Accelerate

someFunc :: IO ()
someFunc = putStrLn "someFunc"
{-
newtype Table repr key val = Table (Table' repr key val)

class Rep rep key val where
  type Table' rep key val

  unsafeIndex :: (Elt key, Elt val) => Acc (Table rep key val)-> Exp key -> Exp val
  insert :: (Elt key, Elt val) => Acc (Table rep key val) -> Exp key -> Exp val -> Acc (Table rep key val)

class Rep rep key val => Filter rep key val where
  type Row rep key val = r | r -> rep key val
  type FilterTarget rep key val

  filter :: (Elt key, Elt val) => (Exp (Row rep key val) -> Exp Bool) -> Acc (Table rep key val) -> Acc (FilterTarget rep key val)

data Dense

instance Shape key => Rep Dense key val where
  type Table' Dense key val = Array key val

  unsafeIndex = (!)

  insert tab k v = undefined

  -- filter f tab = undefined
  -- index table k = match

instance Filter Dense DIM1 val where
  type Row Dense DIM1 val = (DIM1, val)
  type FilterTarget Dense DIM1 val = Table Sparse DIM1 val

data Sparse

instance Shape key => Rep Sparse key val where
  type Table' Sparse key val = (Array key (Maybe DIM1), Vector val)

  unsafeIndex (T2 is vs) k = (is ! k) & match \case
    Just_ i -> vs ! i
    Nothing_ -> error ""

-- (!?) :: Table rep key val -> key -> Maybe val
-}

-- Generic interface.
newtype Table rep key val = Table (TableR rep key val)
newtype Key   rep key     = Key   (KeyR   rep key)
newtype Val   rep     val = Val   (ValR   rep     val)

class Rep rep key val where
  type TableR rep key val
  type KeyR   rep key
  type ValR   rep     val

  unsafeIndex :: Table rep key val -> Key rep key -> Val rep val

-- Accelerate-specific implementation.
data ADense

instance (Shape key, Elt val) => Rep ADense key val where
  type TableR ADense key val = Acc (Array key val)
  type KeyR   ADense key     = Exp key
  type ValR   ADense     val = Exp val

  unsafeIndex (Table arr) (Key key) = Val (arr ! key)

-- Utility function: run the Accelerate computation underlying the table.
runTable :: (TableR rep key val ~ Acc a)
         => (Acc a -> a)
         -> Table rep key val
         -> a
runTable run (Table tab) = run tab

----------

data ASparse2D

instance (Eq key, Elt val) => Rep ASparse2D key val where  
  type TableR ASparse2D key val = Acc (Vector (key, val))
  type KeyR ASparse2D key = Exp key
  type ValR ASparse2D val = Exp val

  unsafeIndex (Table tab) (Key key) =
    let T2 res _   = filter ((== key) . fst) tab
        T2 _   val = res ! I1 0
    in  Val val

class (Rep rep key val, Rep rep' key val') => Map rep rep' key val val' where
  tmap :: (Val rep val -> Val rep' val')
       -> Table rep key val
       -> Table rep' key val'

instance (Shape key, Elt val, Elt val', Rep ADense key val, Rep ADense key val') => Map ADense ADense key val val' where
  tmap f (Table arr) = Table (map (\x -> let (Val x') = f (Val x) in x') arr)