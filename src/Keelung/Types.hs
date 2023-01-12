{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Keelung.Types
  ( Var,
    Addr,
    Width,
    Heap,
    ElemType (..),
    indent,
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

-- | Bit width
type Width = Int

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

--------------------------------------------------------------------------------

-- | Handy function for prettifying stuff
indent :: String -> String
indent = unlines . map ("  " <>) . lines