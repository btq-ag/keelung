{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Keelung.Heap
  ( Addr,
    Heap,
    ElemType (..),
    ArrM (..),
    lengthOf,
  )
where

import Control.DeepSeq (NFData)
import Data.IntMap (IntMap)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Keelung.Syntax (Width)

-- | A mutable array
data ArrM t = ArrayRef ElemType Int Addr
  deriving (Eq)

-- | Length of a mutable array
lengthOf :: ArrM t -> Int
lengthOf ((ArrayRef _ len _)) = len

--------------------------------------------------------------------------------

-- | An "Address" is also just a integer.
type Addr = Int

--------------------------------------------------------------------------------

-- | A Heap is an mapping of mappings of variables
type Heap =
  IntMap
    ( ElemType, -- kind of element
      IntMap Int -- mapping of index to address of element variables
    )

-- | Type of elements of a array
data ElemType
  = ElemF -- Field elements
  | ElemB -- Booleans
  | ElemU Width
  | ElemArr ElemType Int -- Arrays (with type of its elements and its size)
  | EmptyArr -- Currently only empty arrays have this kind
  deriving (Show, Eq, Generic, NFData)

instance Serialize ElemType

instance Semigroup ElemType where
  a <> b = case (a, b) of
    (ElemF, ElemF) -> ElemF
    (ElemB, ElemB) -> ElemB
    (ElemArr a' l, ElemArr b' _) -> ElemArr (a' <> b') l
    (ElemArr a' l, EmptyArr) -> ElemArr a' l
    (EmptyArr, ElemArr b' l) -> ElemArr b' l
    (EmptyArr, EmptyArr) -> EmptyArr
    _ -> error "ElemType must be the same"
