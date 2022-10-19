{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Keelung.Types
  ( Var,
    Addr,
    Heap,
    ElemType (..),
  )
where

import Control.DeepSeq (NFData)
import Data.IntMap (IntMap)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- | A "Variable" is just a integer.
type Var = Int

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
  = NumElem -- Field numbers
  | BoolElem -- Booleans
  | ArrElem ElemType Int -- Arrays (with type of its elements and its size)
  deriving (Show, Eq, Generic, NFData)

instance Serialize ElemType

instance Semigroup ElemType where
  a <> b = case (a, b) of
    (NumElem, NumElem) -> NumElem
    (BoolElem, BoolElem) -> BoolElem
    (ArrElem a' l, ArrElem b' _) -> ArrElem (a' <> b') l
    _ -> error "ElemType must be the same"
