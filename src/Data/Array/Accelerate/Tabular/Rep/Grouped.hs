{-# LANGUAGE NoImplicitPrelude     #-}

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Array.Accelerate.Tabular.Rep.Grouped where

import Data.Array.Accelerate
import Data.Array.Accelerate.Tabular.Rep.Snoc
import Data.Array.Accelerate.Tabular.Classes.Rep
import Data.Coerce
-- GHC can only infer Coercible if the Acc constructor is in scope.
import Data.Array.Accelerate.Smart (Acc(..))
import Lens.Micro
import Data.Array.Accelerate.Tabular.Classes.Index
import Data.Array.Accelerate.Tabular.Classes.Fold

infixl 3 :::
data tail ::: head
  deriving (Generic)

instance (Rep (rep :.: r) (keys :.: key), Elt keys, Elt key) =>
  Rep (rep ::: r) (keys :.: key) where

  type MetaR (rep ::: r) (keys :.: key) = MetaR (rep :.: r) (keys :.: key)

  emptyMeta = coerce (emptyMeta @(rep :.: r) @(keys :.: key))
  createMeta ks = over _1 coerce (createMeta @(rep :.: r) @(keys :.: key) ks)

instance (Index (rep :.: r) (keys :.: key), Elt keys, Elt key) =>
  Index (rep ::: r) (keys :.: key) where

  toLinearIndex met =
    toLinearIndex       @(rep :.: r) (coerce met)
  unsafeToLinearIndex met =
    unsafeToLinearIndex @(rep :.: r) (coerce met)

instance ( Fold (RepFold (rep :.: r) (keys :.: key))
                (KeyFold (rep :.: r) (keys :.: key))
         , Fold (rep :.: r) (keys :.: key)
         , Elt keys
         , Elt key) =>
  Fold (rep ::: r) (keys :.: key) where

  type RepFold (rep ::: r) (keys :.: key) =
    RepFold (RepFold (rep :.: r) (keys :.: key))
            (KeyFold (rep :.: r) (keys :.: key))
  type KeyFold (rep ::: r) (keys :.: key) =
    KeyFold (RepFold (rep :.: r) (keys :.: key))
            (KeyFold (rep :.: r) (keys :.: key))

  foldMeta met = 
    let (met',  seg)  = foldMeta @(rep :.: r) @(keys :.: key) (coerce met)
        (met'', seg') = foldMeta met'
    in  (met'', foldSeg (+) 0 seg seg')
