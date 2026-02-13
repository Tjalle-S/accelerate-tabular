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
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module Data.Array.Accelerate.Tabular where

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Data.Semigroup

import qualified Data.Array.Accelerate.Unsafe as Unsafe


import Prelude (Show (..))
import Data.Array.Accelerate.Data.Maybe


-- Representation class.
-- -------------------------------------------------


class (Elt key, Arrays (MetaR rep key)) => Rep rep key where
  type MetaR rep key

  -- TODO: make this better
  enumKeys :: Acc (Meta rep key) -> Acc (Vector key)
  createMeta :: Acc (Meta rep key)

newtype Meta rep key = Meta (MetaR rep key)
  deriving (Generic)

deriving instance Show (MetaR rep key) => Show (Meta rep key)

instance Arrays (MetaR rep key) => Arrays (Meta rep key)

{-# COMPLETE Meta_ #-}
pattern Meta_ :: Arrays (MetaR rep key)
              => Acc (MetaR rep key)
              -> Acc (Meta rep key)
pattern Meta_ dat = Pattern dat


-- Index class.
-- -------------------------------------------------------------


class (Rep rep key) => Index rep key where
  
  unsafeToLinearIndex :: Acc (Meta rep key)
                      -> Exp key
                      -> Exp Int
  unsafeToLinearIndex met = fromJust . toLinearIndex met

  toLinearIndex :: Acc (Meta rep key)
                -> Exp key
                -> Exp (Maybe Int)
  toLinearIndex met = Just_ . unsafeToLinearIndex met

  {-# MINIMAL unsafeToLinearIndex | toLinearIndex #-}


-- Table type.
-- ---------------------------------------------------------------


data Table rep key val = Table {
  meta :: Meta rep key
, vals :: Vector val
} deriving (Generic)

deriving instance (Show (MetaR rep key), Elt val, Show val) => Show (Table rep key val)

instance (Arrays (MetaR rep key), Elt val) => Arrays (Table rep key val)

{-# COMPLETE Table_ #-}
pattern Table_ :: (Arrays (MetaR rep key), Elt val)
               => Acc (Meta rep key)
               -> Acc (Vector val)
               -> Acc (Table rep key val)
pattern Table_ { meta_, vals_ } = Pattern (meta_, vals_)


-- Table expansion (:.:) type.
-- -------------------------------------------------------------


data tail :.: head = tail :.: head
  deriving (Generic)
infixl 3 :.:

instance (Elt tail, Elt head) => Elt (head :.: tail)

instance (Show tail, Show head) => Show (tail :.: head) where
  show (t :.: h) = show t <> " :.: " <> show h

mkPattern ''(:.:)

-- -- instance (Eq tail, Eq head) => Eq (tail :.: head) where
-- --   (t1 ::.: h1) == (t2 ::.: h2) = h1 == h2 && t1 == t2

-- -- instance (Ord tail, Ord head) => Ord (tail :.: head) where
-- --   compare (t1 ::.: h1) (t2 ::.: h2) = compare t1 t2 <> compare h1 h2

-- -- instance Semigroup (Exp Ordering) where
-- --   EQ_ <> y = y
-- --   x   <> _ = x


-- Empty table instances.
-- ---------------------------------------------------------------


instance Rep Z Z where
  type MetaR Z Z = ()

  enumKeys _ = flatten (unit Z_)
  createMeta = Meta_ (lift ())

instance Index Z Z where
  unsafeToLinearIndex _ _ = 0
  toLinearIndex       _ _ = Just_ 0


-- Dense representation instances.
-- --------------------------------------------------------------


data Dense

instance (Rep rep key) => Rep (rep :.: Dense) (key :.: Int) where
  type MetaR (rep :.: Dense) (key :.: Int) =
    (Scalar Int, Meta rep key)

  enumKeys (Meta_ (T2 n met)) = expand (const $ the n) (::.:) (enumKeys met)
  createMeta = Meta_ $ T2 (unit 0) createMeta

instance (Index rep key) => Index (rep :.: Dense) (key :.: Int) where
  unsafeToLinearIndex (Meta_ (T2 n met)) (k ::.: i) =
    the n * unsafeToLinearIndex met k + i
  toLinearIndex       (Meta_ (T2 n met)) (k ::.: i) = 
    fmap (\i' -> the n * i' + i) (toLinearIndex met k)


-- Compressed representation instances.
-- ----------------------------------------------------------------


data Compressed

instance (Rep rep key) => Rep (rep :.: Compressed) (key :.: Int) where
  type MetaR(rep :.: Compressed) (key :.: Int) =
    (Vector Int, Vector Int, Meta rep key)

  enumKeys (Meta_ (T3 seg ks met)) =
    expand (getCount . fst) makeKey (indexed $ enumKeys met)
      where
        getCount (I1 i) = let next = seg !! (i + 1)
                              curr = seg !! i
                          in  next - curr

        makeKey (T2 n k) i = let i' = (seg ! n) + i
                             in  k ::.: (ks !! i')

  createMeta = let seg = fill (I1 2) 0
                   ks  = fill (I1 0) Unsafe.undef
               in  Meta_ (T3 seg ks createMeta)


-- User-facing functions.
-- -----------------------------------------------------


project :: (Rep rep key, Elt val, Elt val')
        => (Exp val -> Exp val')
        -> Acc (Table rep key val)
        -> Acc (Table rep key val')
project f Table_ { meta_, vals_ } = Table_ meta_ (map f vals_)

index :: (Index rep key, Elt val)
      => Acc (Table rep key val)
      -> Exp key
      -> Exp (Maybe val)
index Table_ { meta_, vals_ } k = fmap (vals_ !!) (toLinearIndex meta_ k)

unsafeIndex :: (Index rep key, Elt val)
            => Acc (Table rep key val)
            -> Exp key
            -> Exp val
unsafeIndex Table_ { meta_, vals_ } k = vals_ !! unsafeToLinearIndex meta_ k

unsafeUpdate :: (Index rep key, Elt val)
             => (Exp key -> Exp val -> Exp val)
             -> Acc (Vector key)
             -> Acc (Table rep key val)
             -> Acc (Table rep key val)
unsafeUpdate f ks Table_ { meta_, vals_ } = 
  let idxs = map (unsafeToLinearIndex meta_) ks
      ks'  = scatter idxs (fill (shape vals_) Nothing_) (map Just_ ks)
      vs   = zipWith f' vals_ ks'
  in  Table_ { meta_ = meta_, vals_ = vs }
  where
    f' v mk = match mk & \case
      Nothing_ -> v
      Just_ k  -> f k v

emptyTable :: (Rep rep key, Elt val) => Acc (Table rep key val)
emptyTable = Table_ {
  meta_ = createMeta
, vals_ = fill (I1 0) Unsafe.undef
}

createTable :: (Insert rep key, Elt val)
            => Acc (Vector (key, val))
            -> Acc (Table rep key val)
createTable kvs = 
  let (ks, vs)     = unzip kvs
      T2 meta perm = create ks
      vals         = gather perm vs
  in  Table_ { meta_ = meta, vals_ = vals }
-- -----------------------------------------------------


-- class (Rep rep key) => Construct rep key where
--   construct :: Acc (Vector key) -> Acc (Meta rep key, Vector Int)

-- instance Construct Z Z where
--   construct ks = T2 emptyMeta (enumFromN (shape ks) 0)


-- instance (Construct rep key) => Construct (rep :.: Dense) (key :.: Int) where
--   construct ks = let (ks', is) = splitKeys ks
--                      T2 m p    = construct ks'
--                      m' = T2 (maximum is) m
--                  in  T2 (Meta_ m') undefined

-- class (Rep rep key) => Construct rep key where
--   construct :: 



-- testTable :: Acc (Table (Z :.: Dense :.: Dense) (Z :.: Int :.: Int) Char)
-- testTable =
--   let dat  = use $ fromList (Z :. 26) ['a' .. 'z']
--       tab0 = Table_ dat
--       tab1 = Table_ (T2 (unit 2) tab0)
--   in Table_ (T2 (unit 13) tab1)

-- type CSR   = Z :.: Dense :.: Compressed
type DIM2' = Z :.: Int :.: Int

-- testTable2 :: Acc (Table CSR DIM2' Char)
-- testTable2 = 
--   let dat  = use $ fromList (Z :. 26) ['a' .. 'z']
--       tab0 = Table_ dat
--       tab1 = Table_ (T2 (unit 2) tab0)
      
--       seg  = use $ fromList (Z :. 3) [0, 13, 26]
--       idx  = generate (Z_ ::. 26) (\(I1 i) -> i `mod` 13)
--   in  Table_ (T3 seg idx tab1)

-- class (Rep rep key val) => Construct rep key val where
--   construct :: Exp key -- ^ Extent.
--             -> Acc (Vector val)
--             -> Acc (Table rep key val)
--             -> Acc (Vector key)

-- instance (Rep rep key val) => Construct (rep :.: Dense) (key :.: Int) val where
--   construct (ext ::.: e) ks vs =
--     let ks = undefined
--     in  Table_ (T2 (unit e) undefined)

class (Rep rep key) => Insert rep key where
  insert_ :: Acc (Vector key) -> Acc (Meta rep key) -> Acc (Meta rep key, Vector Int)

  create :: Acc (Vector key) -> Acc (Meta rep key, Vector Int)
  create ks = insert_ ks createMeta



type CC = Z :.: Compressed :.: Dense

testTable3 :: Acc (Table CC DIM2' Char)
testTable3 =
  let dat  = use $ fromList (Z :. 26) ['a' .. 'z']
      met0 = createMeta

      seg = use $ fromList (Z :. 2) [0, 2]
      idx = use $ fromList (Z :. 2) [0, 1]
      met1 = Meta_ (T3 seg idx met0)

      met2 = Meta_ (T2 (unit 13) met1)
  in Table_ { meta_ = met2, vals_ = dat }


splitKeys :: (Elt k, Elt i) => Acc (Vector (k :.: i))
          -> (Acc (Vector k), Acc (Vector i))
splitKeys = unzip . map (\(k ::.: i) -> T2 k i)


