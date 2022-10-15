{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Keelung.Types where

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

--------------------------------------------------------------------------------

-- | Variable bookkeeping
data VarCounters = VarCounters
  { -- Size of input variables
    varInput :: Int,
    -- Size of output variables
    varOutput :: Int,
    -- Size of other ordinary variables
    varOrdinary :: Int
  }
  deriving (Generic, NFData, Eq)

instance Serialize VarCounters

instance Show VarCounters where
  show counters@(VarCounters input output _) =
    "total variable size: " <> show (totalVarSize counters)
      <> showInputVars
      <> showOutputVars
    where
      showInputVars = case input of
        0 -> ""
        1 -> "\ninput variables: $0"
        _ -> "\ninput variables: $0 .. $" <> show (input - 1)

      showOutputVars = case output of
        0 -> ""
        1 -> "\noutput variables: $" <> show input
        _ -> "\noutput variables: $" <> show input <> " .. $" <> show (input + output - 1)

instance Semigroup VarCounters where
  a <> b =
    VarCounters
      { varInput = varInput a + varInput b,
        varOutput = varOutput a + varOutput b,
        varOrdinary = varOrdinary a + varOrdinary b
      }

instance Monoid VarCounters where
  mempty = VarCounters 0 0 0 

bumpInputVar :: VarCounters -> VarCounters
bumpInputVar counters = counters {varInput = varInput counters + 1}

bumpOrdinaryVar :: VarCounters -> VarCounters
bumpOrdinaryVar counters = counters {varOrdinary = varOrdinary counters + 1}

indent :: String -> String
indent = unlines . map ("  " <>) . lines

totalVarSize :: VarCounters -> Int
totalVarSize counter = varInput counter + varOutput counter + varOrdinary counter