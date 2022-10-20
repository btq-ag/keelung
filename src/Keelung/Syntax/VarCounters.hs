{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Keelung.Syntax.VarCounters
  ( VarCounters (VarCounters),
    totalVarSize,
    boolInputVarSize,
    numInputVarSize,
    pinnedVarSize,
    inputVarSize,
    outputVarSize,
    ordinaryVarSize,
    ----
    bumpNumInputVar,
    bumpBoolInputVar,
    bumpOrdinaryVar,
    setOutputVarSize,
    setOrdinaryVarSize,
    setNumWidth,
    getNumWidth,
    ----
    indent,
    ----
    getBitVar,
    distinguishInputVar,
  )
where

import Control.DeepSeq (NFData)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

-- | Variable bookkeeping
data VarCounters = VarCounters
  { -- Size of Boolean input variables
    varBoolInput :: !Int,
    -- Size of Number input variables
    varNumInput :: !Int,
    -- Intervals of Boolean input variables
    varBoolIntervals :: !BoolIntervals,
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
  show counters =
    "total variable size: "
      <> show (totalVarSize counters)
      <> showInputVars
      <> showOutputVars
    where
      showInputVars = case inputVarSize counters of
        0 -> ""
        1 -> "\ninput variables: $0"
        n -> "\ninput variables: $0 .. $" <> show (n - 1)

      showOutputVars = case outputVarSize counters of
        0 -> ""
        1 -> "\noutput variables: $" <> show (inputVarSize counters)
        n -> "\noutput variables: $" <> show (inputVarSize counters) <> " .. $" <> show (inputVarSize counters + n - 1)

instance Semigroup VarCounters where
  a <> b =
    VarCounters
      { varBoolInput = varBoolInput a + varBoolInput b,
        varNumInput = varNumInput a + varNumInput b,
        varBoolIntervals = varBoolIntervals a <> varBoolIntervals b,
        varNumWidth = max (varNumWidth a) (varNumWidth b),
        varOutput = varOutput a + varOutput b,
        varOrdinary = varOrdinary a + varOrdinary b
      }

instance Monoid VarCounters where
  mempty = VarCounters 0 0 mempty 0 0 0

--------------------------------------------------------------------------------

-- | Total size of all variables
totalVarSize :: VarCounters -> Int
totalVarSize counters = inputVarSize counters + varOutput counters + varOrdinary counters

-- | Calculate the size of variables that are considered "pinned"
--   i.e. they should not be modified by optimizers
pinnedVarSize :: VarCounters -> Int
pinnedVarSize counters = inputVarSize counters + outputVarSize counters

-- | Size of input variables
--      = size of Boolean input variables
--      + size of Number input variables
--      + size of binary representation of Number input variables
inputVarSize :: VarCounters -> Int
inputVarSize counters = varBoolInput counters + (1 + varNumWidth counters) * varNumInput counters

outputVarSize :: VarCounters -> Int
outputVarSize = varOutput

ordinaryVarSize :: VarCounters -> Int
ordinaryVarSize = varOrdinary

boolInputVarSize :: VarCounters -> Int
boolInputVarSize = varBoolInput

numInputVarSize :: VarCounters -> Int
numInputVarSize = varNumInput

--------------------------------------------------------------------------------

-- | Return the variable index of the bit of a Number input variable
getBitVar :: VarCounters -> Int -> Int -> Maybe Int
getBitVar counters inputVarIndex bitIndex = (+) bitIndex <$> getNumInputIndex
  where
    getNumInputIndex :: Maybe Int
    getNumInputIndex = case distinguishInputVar counters inputVarIndex of
      Left _ -> Nothing
      Right n -> Just $ varBoolInput counters + varNumInput counters + n

--------------------------------------------------------------------------------

-- | Bump the Number input variable counter
bumpNumInputVar :: VarCounters -> VarCounters
bumpNumInputVar counters =
  counters
    { varNumInput = varNumInput counters + 1
    }

-- | Bump the Boolean input variable counter
bumpBoolInputVar :: VarCounters -> VarCounters
bumpBoolInputVar counters =
  counters
    { varBoolInput = varBoolInput counters + 1,
      varBoolIntervals = intervals'
    }
  where
    inputVarIndex = varBoolInput counters + varNumInput counters
    intervals = varBoolIntervals counters
    intervals' = case IntMap.lookupMax intervals of
      -- no existing Boolean intervals, create a new one
      Nothing -> IntMap.singleton inputVarIndex (0, 1)
      -- found an existing Boolean interval, see if we can extend it
      Just (lastIntervalIndex, (boolVarsBefore, lastIntervalSize)) ->
        if inputVarIndex < lastIntervalIndex + lastIntervalSize
          then -- within the last interval, extend it
            IntMap.insert lastIntervalIndex (boolVarsBefore, lastIntervalSize + 1) intervals
          else -- create a new interval
            IntMap.insert inputVarIndex (boolVarsBefore + lastIntervalSize, 1) intervals

-- | Bump the output variable counter
bumpOrdinaryVar :: VarCounters -> VarCounters
bumpOrdinaryVar counters = counters {varOrdinary = varOrdinary counters + 1}

setOutputVarSize :: Int -> VarCounters -> VarCounters
setOutputVarSize size counters = counters {varOutput = size}

setOrdinaryVarSize :: Int -> VarCounters -> VarCounters
setOrdinaryVarSize size counters = counters {varOrdinary = size}

-- | Updates the width of binary representation of a Number when it's known
setNumWidth :: Int -> VarCounters -> VarCounters
setNumWidth width counters = counters {varNumWidth = width}

-- | Get the width of binary representation of a Number
getNumWidth :: VarCounters -> Int
getNumWidth = varNumWidth

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
type IntervalLength = Int

type BoolIntervals = IntMap (Int, IntervalLength)

-- | Given a variable index,
--      returns Left along with the number of Boolean variables before it if it's a Boolean variable
--      returns Right along with the number of Number variables before it if it's a Number variable
distinguishInputVar :: VarCounters -> Int -> Either Int Int
distinguishInputVar counters varIndex = case IntMap.lookupLE varIndex (varBoolIntervals counters) of
  Nothing ->
    -- there are no Boolean variables before this variable: Number
    Right varIndex
  Just (start, (before, size)) ->
    if varIndex < start + size
      then Left (before + varIndex - start) -- within an interval: Boolean
      else Right (varIndex - before - size) -- after an interval: Number

-- | Inverse of 'distinguishInputVar'
-- mixInputVar :: BoolIntervals -> Either Int Int -> Int
-- mixInputVar intervals (Left boolVarIndex) = case IntMap.lookupLE boolVarIndex intervals of
--   Nothing -> error "mixInputVar: invalid Boolean variable index"
--   Just (start, (before, size)) ->
--     if boolVarIndex < start + size
--       then before + boolVarIndex - start
--       else error "mixInputVar: invalid Boolean variable index"
-- mixInputVar intervals (Right numVarIndex) = case IntMap.lookupLE numVarIndex intervals of
--   Nothing -> numVarIndex
--   Just (start, (before, size)) ->
--     if numVarIndex < start + size
--       then error "mixInputVar: invalid Number variable index"
--       else before + numVarIndex + size
