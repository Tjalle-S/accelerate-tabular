{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeOperators         #-} 
{-# LANGUAGE PatternSynonyms       #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}

module Data.Array.Accelerate.Tabular.Rep.Hashed (
  Hashed
) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.HashMap ( Hashable )

import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Array.Accelerate.Tabular.Rep.Snoc
import Data.Array.Accelerate.Tabular.Util

-- | Stores keys in a hash map.
--
data Hashed

instance (Rep rep keys, Hashable key) =>
  Rep (rep :.: Hashed) (keys :.: key) where

  type MetaR (rep :.: Hashed) (keys :.: key) =
    (Meta rep keys, Scalar Int, Vector key)

  emptyMeta = HashedMeta emptyMeta (unit 0) emptyVector
  
-- Local utilities.
-- ----------------

pattern HashedMeta :: (Arrays (Meta rep keys), Hashable key)
                       => Acc (Meta rep keys)
                       -> Acc (Scalar Int)
                       -> Acc (Vector key)
                       -> Acc (Meta (rep :.: Hashed) (keys :.: key))
pattern HashedMeta { met, width, ks } = Meta_ (T3 met width ks)
{-# COMPLETE HashedMeta #-}
