{-# LANGUAGE NoImplicitPrelude    #-}

{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE ConstraintKinds      #-}  

{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DataKinds            #-}

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE ExplicitForAll #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Array.Accelerate.Tabular.Prelude.Reindex (
  reindex, reindexUnique, reindex'

) where

import Prelude (id)

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Data.Maybe

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Prelude.Table
import Data.Array.Accelerate.Tabular.Util

import Lens.Micro
import Data.Array.Accelerate.Data.Lens ()

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
-- or pass 'const' and 'id'.
--
reindex :: (NotScalar key', Rep rep key, Rep rep' key', Elt val)
        => (Exp val -> Exp val -> Exp val)
        -> (Exp key -> Exp key')
        -> Acc (Table rep  key val)
        -> Acc (Table rep' key' val)
reindex combv mapk Table_ { meta_, vals_ } =
  let (ks', vs') = unzip 
                 $ afst
                 $ justs
                 $ map (fmap $ over _1 mapk)
                 $ zipWithChecked
                     (\k v -> maybe Nothing_ (Just_ . T2 k) v)
                     (enumKeys meta_)
                     vals_
      T3 met perm n = createMeta NoAssumeOrdered ks'
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
reindexUnique :: forall rep' key' rep key val
              .  (NotScalar key', Rep rep key, Rep rep' key', Elt val)
              => (Exp key -> Exp key')
              -> Acc (Table rep  key val)
              -> Acc (Table rep' key' val)
reindexUnique = reindex const

-- | Change the underlying representation of a table.
--
reindex' :: (NotScalar key, Rep rep key, Rep rep' key, Elt val)
         => Acc (Table rep  key val)
         -> Acc (Table rep' key val)
reindex' = reindex const id
