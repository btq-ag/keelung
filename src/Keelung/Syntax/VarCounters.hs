{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Keelung.Syntax.VarCounters
  ( VarCounters (VarCounters),
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
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap



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

-- | Return the variable index of the bit of a Number input variable
-- getBitVar :: VarCounters -> Int -> Int -> Maybe Int
-- getBitVar counters inputVarIndex bitIndex = (+) bitIndex <$> getNumInputIndex counters inputVarIndex

-- getNumInputIndex :: VarCounters -> Int -> Maybe Int
-- getNumInputIndex 

--   guard (inputVarIndex < varNumInput counters)
--   guard (bitIndex < varNumWidth counters)
--   return $ varInput counters + inputVarIndex * varNumWidth counters + bitIndex

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

-- | Data structure for storing intervals of Boolean variables
--   For fast lookup (O(log n)) of whether a variable is Boolean or Number
--      and how many Boolean or Number variables are before it
--
--   The index is for marking the beginning of an interval
--   The value indicates:
--          1. the number of Boolean variables before this interval
--          2. the size of this interval
--
--   For example: 
--      nnnnnnnnnnn     => []
--      bbbnnn          => [(0, (0, 3))]
--      nbbbnnn         => [(1, (0, 3)]
--      nbbbnnnbb       => [(1, (0, 3)), (7, (3, 2))]
--      nbbbnnbbnnnnbb  => [(1, (0, 3)), (7, (3, 2)), (12, (5, 2))]
--

type IntervalLength = Int
type BoolVarIntervals = IntMap (Int, IntervalLength) 

-- | Given a variable index, 
--      returns Left along with the number of Boolean variables before it if it's a Boolean variable
--      returns Right along with the number of Number variables before it if it's a Number variable
distinguishInputVar :: BoolVarIntervals -> Int -> Either Int Int
distinguishInputVar intervals varIndex = case IntMap.lookupLE varIndex intervals of
  Nothing -> -- there are no Boolean variables before this variable: Number
    Right varIndex
  Just (start, (before, size)) -> if varIndex < start + size
    then Left (before + varIndex - start) -- within an interval: Boolean
    else Right (varIndex - before - size) -- after an interval: Number

-- | Returns the number of Boolean variables
totalBoolVarSize :: BoolVarIntervals -> Int
totalBoolVarSize = sum . map snd . IntMap.elems