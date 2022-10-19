{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Keelung.Types
  ( Var,
    Addr,
    Heap,
    ElemType (..),
    ----
    VarCounters (VarCounters),
    totalVarSize,
    inputVarSize,
    outputVarSize,
    ordinaryVarSize,
    pinnedVarSize,
    numInputVarSize,
    ----
    bumpNumInputVar,
    bumpBoolInputVar,
    bumpOrdinaryVar,
    setOutputVarSize,
    setOrdinaryVarSize,
    setNumWidth,
    ----
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
    varInput :: !Int,
    -- Size of Number input variables
    -- (so that we can allocate variables for binary representation of these variables)
    varNumInput :: !Int,
    -- Width of binary representation of Numbers
    varNumWidth :: !Int,
    -- Size of output variables
    varOutput :: !Int,
    -- Size of other ordinary variables
    varOrdinary :: !Int
  }
  deriving (Generic, NFData, Eq)

instance Serialize VarCounters

instance Show VarCounters where
  show counters@(VarCounters input _ _ output _) =
    "total variable size: " <> show (totalVarSize counters)
      <> show (totalVarSize counters)
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
        varNumInput = varNumInput a + varNumInput b,
        varNumWidth = max (varNumWidth a) (varNumWidth b),
        varOutput = varOutput a + varOutput b,
        varOrdinary = varOrdinary a + varOrdinary b
      }

instance Monoid VarCounters where
  mempty = VarCounters 0 0 0 0 0

--------------------------------------------------------------------------------

-- | Total size of all variables
totalVarSize :: VarCounters -> Int
totalVarSize counters = varInput counters + varOutput counters + varOrdinary counters

-- | Calculate the size of variables that are considered "pinned"
--   i.e. they should not be modified by optimizers
pinnedVarSize :: VarCounters -> Int
pinnedVarSize counters = inputVarSize counters + outputVarSize counters

-- | Size of input variables
--      = size of input variables
--      + size of binary representation of Number input variables
inputVarSize :: VarCounters -> Int
inputVarSize counters = varInput counters + varNumWidth counters * varNumInput counters

outputVarSize :: VarCounters -> Int
outputVarSize = varOutput

ordinaryVarSize :: VarCounters -> Int
ordinaryVarSize = varOrdinary

numInputVarSize :: VarCounters -> Int
numInputVarSize = varNumInput

--------------------------------------------------------------------------------

-- | Bump the input variable & number input variable counter
bumpNumInputVar :: VarCounters -> VarCounters
bumpNumInputVar counters =
  counters
    { varInput = varInput counters + 1,
      varNumInput = varNumInput counters + 1
    }

-- | Bump the input variable counter
bumpBoolInputVar :: VarCounters -> VarCounters
bumpBoolInputVar counters = counters {varInput = varInput counters + 1}

-- | Bump the output variable counter
bumpOrdinaryVar :: VarCounters -> VarCounters
bumpOrdinaryVar counters = counters {varOrdinary = varOrdinary counters + 1}

setOutputVarSize :: Int -> VarCounters -> VarCounters
setOutputVarSize size counters = counters {varOutput = size}

setOrdinaryVarSize :: Int -> VarCounters -> VarCounters
setOrdinaryVarSize size counters = counters {varOrdinary = size}

setNumWidth :: Int -> VarCounters -> VarCounters
setNumWidth width counters = counters {varNumWidth = width}

--------------------------------------------------------------------------------

-- | Handy function for prettifying VarCounters
indent :: String -> String
indent = unlines . map ("  " <>) . lines

--------------------------------------------------------------------------------
