module Data.Array.Accelerate.Tabular.Rep (
  module Dense
, module Compressed
, module Offset
, module Hashed
, module Range
, module Singleton
, module Rep
) where

import Data.Array.Accelerate.Tabular.Rep.Dense      as Dense
import Data.Array.Accelerate.Tabular.Rep.Compressed as Compressed
import Data.Array.Accelerate.Tabular.Rep.Offset     as Offset
import Data.Array.Accelerate.Tabular.Rep.Hashed     as Hashed
import Data.Array.Accelerate.Tabular.Rep.Range      as Range
import Data.Array.Accelerate.Tabular.Rep.Singleton  as Singleton
import Data.Array.Accelerate.Tabular.Classes.Rep    as Rep