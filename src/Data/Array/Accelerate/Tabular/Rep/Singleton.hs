{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE NamedFieldPuns        #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module Data.Array.Accelerate.Tabular.Rep.Singleton (
  UnsafeCompleteSingleton
) where

import Data.Array.Accelerate

import Data.Array.Accelerate.Tabular.Classes.Rep

import Data.Array.Accelerate.Tabular.Util
import Data.Array.Accelerate.Unsafe (undef)
import Data.Array.Accelerate.Tabular.Classes.Fold

import Prelude (type (~))

import Data.Proxy
import Data.Array.Accelerate.Control.Monad
import qualified Prelude as P


-- | Stores a single key on this level for each key in the parent levels.
--
data Singleton

-- | Stores a single key on this level for each key in the parent levels.
-- Assumes that there is exactly 1 child key for each parent.
--
-- Note that no additional checks are performed. In particular:
--
-- - If a single parent has multiple child keys, only 1 is stored, non-deterministically.
-- - If a parent key has no child keys, the key stored is undefined.
--
data UnsafeCompleteSingleton

-- Singleton instances.
-- --------------------

-- instance (Rep rep keys, Elt key) => Rep (rep :. Singleton) (keys :. key) where

--   type MetaR (rep :. Singleton) (keys :. key) = (Meta rep keys, Vector key)

--   emptyMeta = SingletonMeta emptyMeta emptyVector


-- Unsafe Singleton instances.
-- ---------------------------

instance (Rep rep keys, Eq key) =>
  Rep (rep :. UnsafeCompleteSingleton) (keys :. key) where

    type MetaR (rep :. UnsafeCompleteSingleton) (keys :. key) =
      (Meta rep keys, Vector key)

    type Ordered (rep :. UnsafeCompleteSingleton) = Ordered rep
    type FastIndex (rep :. UnsafeCompleteSingleton) = FastIndex rep

    getIndexConstraint _ _ =
      case getIndexConstraint (Proxy @rep) (Proxy @keys) of
        NoDict -> NoDict
        Dict   -> Dict

    emptyMeta = Meta_ $ T2 emptyMeta emptyVector

    createMeta o ks =
      let
        (ks', is)     = splitKeys ks
        T3 met perm n = createMeta o ks'

        perm' = map unindex1 perm
        target = fill (I1 $ the n) undef

        met' = Meta_ $ T2 met (scatter perm' target is)
      in T3 met' perm n

    enumKeys SingletonMeta { met, ks } = zipWithChecked (::.) (enumKeys met) ks


instance (Fold rep keys, Eq key) => Fold (rep :. UnsafeCompleteSingleton) (keys :.key) where

  foldMeta d dmet@(Meta_ (T2 met _)) =
    case d of
      FKeep -> let (res, _) = foldMeta FKeep met
               in  (T2 dmet (asnd res), Dict')
      FGroup rest -> foldMeta rest met
      
      --foldMeta rest met
      -- FGroup rest -> foldMeta rest met

    -- case d of
    --   FKeep -> let (res, prf) = foldMeta FKeep met 
    --            in  undefined
              --  in  case prf of
              --        Dict' -> let seg = asnd res
              --                 in  (T2 dmet seg, Dict')

-- instance (Fold rep keys, Eq key) =>
--   Fold (rep :. UnsafeCompleteSingleton) (keys :. key) where

--   foldMeta d dmet@SingletonMeta { met } =
--     case d of
--       FKeep       -> let (res, prf) = foldMeta FKeep met
--                      in  case prf of
--                            Dict' -> let seg = asnd res
--                                     in  (T2 dmet seg, Dict')
--         -- (T2 dmet (asnd $ P.fst $ foldMeta FKeep met), Dict')
--       FGroup rest -> let (res, prf) = foldMeta rest met
--                      in  case prf of
--                            Dict' -> (res, Dict')
        -- foldMeta rest met
  

instance (Index rep keys, Eq key) =>
  Index (rep :. UnsafeCompleteSingleton) (keys :. key) where

  toLinearIndex SingletonMeta { met, ks } (k ::. i) =
    let i' = toLinearIndex met k
    in  f =<< i'
    where
      f i' = if (ks !! i') == i
               then Just_ i'
               else Nothing_

  unsafeToLinearIndex SingletonMeta { met } (k ::. _) =
    unsafeToLinearIndex met k

  toLinearIndices SingletonMeta { met, ks } keys = 
    let (ks', is) = splitKeys keys
        is' = toLinearIndices met ks'
    in  zipWith f is' is
    where
      f mi' i = do
        i' <- mi'
        if (ks !! i') == i
          then Just_ i'
          else Nothing_
    
  unsafeToLinearIndices SingletonMeta { met } keys = 
    let (ks, _) = splitKeys keys
    in  unsafeToLinearIndices met ks




-- Local utilities.
-- ----------------

type SingletonMetaR rep keys key = (Meta rep keys, Vector key)

type IsSingleton rep r keys key =
  ( Rep   (rep :. r) (keys :. key)
  , MetaR (rep :. r) (keys :. key) ~ SingletonMetaR rep keys key
  )

pattern SingletonMeta :: ( IsSingleton rep r keys key
                         , Arrays (Meta rep keys)
                         , Elt key
                         )
                      => Acc (Meta rep keys)
                      -> Acc (Vector key)
                      -> Acc (Meta (rep :. r) (keys :. key))
pattern SingletonMeta { met, ks } = Meta_ (T2 met ks)
{-# COMPLETE SingletonMeta #-}
