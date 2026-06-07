{-# LANGUAGE NoImplicitPrelude  #-}

{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Data.Array.Accelerate.Tabular (
  Table (..)

, Rep

, Z (..), (:.) (..)

-- ** Available level formats
, Dense
, OrdCompressed, NonUniqueCompressed
, UnsafeCompleteSingleton
, Hashed

-- ** Using custom key types.
, SugarR
, Sugar
, Id, G

-- ** 0-dimensional (scalar) tables.
, Scalar
, the, unit

-- ** Introducing tables
, emptyTable
, createTable, orderedCreateTable
, A.use

-- ** Indexing into tables
, IndexKey
, index, (!?), indexMany
, unsafeIndex, (!), unsafeIndexMany

, map
, filter
, assocs, keys, values

-- ** Zipping/joining tables
, innerjoin, leftouterjoin, rightouterjoin, fullouterjoin

-- ** Folding over tables
, Fold
, fold, fold1
, foldAll, fold1All
-- *** Non-commutative folds.
-- If the order in which keys are stored in phyical memory is predictable,
-- folds with non-commutative combination functions can be used as well.
-- These may however be less efficient than using a commutative fold.
, foldNonCommutative, fold1NonCommutative
-- *** Describing folds
, Keep (..), Group (..)
, FoldDescriptor
, FoldResult
-- **** Commonly used fold descriptors
, inner, inner1, inner2, inner3

-- ** Slicing tables
, slice
, SliceFix (..), pattern Slice_
, SliceDescriptor
, SliceResult

-- ** Reindexing tables
, reindex, reindexUnique, reindex'

-- ** Flow control
, (?|), acond
, awhile, awhileSpeculative, awhileAndOne,
  IfThenElse(..)
-- ---------------------------------------------------------------------------
  -- * The /Accelerate/ Expression Language
  -- ** Scalar data types
, Exp

  -- ** SIMD vectors
, Vec, VecElt

-- ** Type classes
-- *** Basic type classes
, Eq(..)
, Ord(..), Ordering(..), pattern LT_, pattern EQ_, pattern GT_
, Enum, succ, pred
, Bounded, minBound, maxBound

-- *** Numeric type classes
, Num, (+), (-), (*), negate, abs, signum, fromInteger
, Integral, quot, rem, div, mod, quotRem, divMod
, Rational(..)
, Fractional, (/), recip, fromRational
, Floating
, pi, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh
, exp, sqrt, log, (**), logBase
, RealFrac(..), div', mod', divMod'
, RealFloat(..)

-- *** Numeric conversion classes
, FromIntegral(..)
, ToFloating(..)

-- ** Lifting and Unlifting
-- $lifting_and_unlifting
--
, Lift(..), Unlift(..)
, lift1, lift2, lift3
, ilift1, ilift2, ilift3

-- ** Pattern synonyms
-- $pattern_synonyms
--
, pattern Pattern
, pattern T2,  pattern T3,  pattern T4,  pattern T5,  pattern T6
, pattern T7,  pattern T8,  pattern T9,  pattern T10, pattern T11
, pattern T12, pattern T13, pattern T14, pattern T15, pattern T16

, pattern Z_, pattern Ix, pattern (::.){-, pattern All_, pattern Any_ -}
, pattern I0, pattern I1, pattern I2, pattern I3, pattern I4
, pattern I5, pattern I6, pattern I7, pattern I8, pattern I9

, pattern Vec2, pattern V2
, pattern Vec3, pattern V3
, pattern Vec4, pattern V4
, pattern Vec8, pattern V8
, pattern Vec16, pattern V16

, mkPattern, mkPatterns

-- ** Scalar operations
-- *** Introduction
, constant

-- *** Tuples
, fst, afst, snd, asnd, curry, uncurry

-- *** Flow control
, (?), match, cond, select, while, iterate
, assert, assertMessage

-- *** Logical operations
, (&&), (&&!), (||), (||!), not

-- *** Numeric operations
, subtract, even, odd, gcd, lcm, (^), (^^)

-- -- *** Shape manipulation
-- index0, index1, unindex1, index2, unindex2, index3, unindex3,
-- indexHead, indexTail,
-- toIndex, fromIndex,
-- intersect,
-- TODO: Key manipulation ?

-- *** Conversions
, ord, chr, boolToInt, bitcast

-- ---------------------------------------------------------------------------
-- * Foreign Function Interface (FFI)
, foreignAcc
, foreignExp

-- ---------------------------------------------------------------------------
-- -- * Plain arrays
-- -- ** Operations
-- arrayRank, arrayShape, arraySize, arrayReshape,
-- indexArray, linearIndexArray,

-- -- ** Getting data in
-- -- $getting_data_in

-- -- *** Function
-- fromFunction,
-- fromFunctionM,

-- -- *** Lists
-- , fromList, toList

-- ---------------------------------------------------------------------------
-- * Useful re-exports
, (.), ($), (&), flip, error, undefined, P.id, const, otherwise
, Show, Generic, HasCallStack
, fromString -- -XOverloadedStrings
-- , fromListN  -- -XOverloadedLists

, type (~)

-- ---------------------------------------------------------------------------
-- Types
, Int, Int8, Int16, Int32, Int64
, Word, Word8, Word16, Word32, Word64
, Half(..), Float, Double
, Bool(..),   pattern True_,    pattern False_
, Maybe(..),  pattern Nothing_, pattern Just_
, Either(..), pattern Left_,    pattern Right_
, Char

, CFloat, CDouble
, CShort, CUShort, CInt, CUInt, CLong, CULong, CLLong, CULLong
, CChar, CSChar, CUChar

, Acc, Arrays, Elt
) where

import Prelude (type (~))
import qualified Prelude as P

import Data.Array.Accelerate hiding (
    map, filter
  , fold, fold1, foldAll, fold1All
  , slice
  , (!)
  , Scalar, unit, the
  )
import qualified Data.Array.Accelerate as A

import Data.Array.Accelerate.Tabular.Prelude
import Data.Array.Accelerate.Tabular.Rep

import Data.Array.Accelerate.Tabular.Classes.IndexKey
import Data.Array.Accelerate.Tabular.Classes.Slice
import Data.Array.Accelerate.Tabular.Rep.Sugar
import Data.Array.Accelerate.Tabular.Classes.Sugar
