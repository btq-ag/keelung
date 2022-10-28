{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Keelung.Syntax.VarCounters
  ( VarCounters,
    InputVar (..),
    makeVarCounters,
    getInputSequence,
    -- Sizes of different variable groups
    totalVarSize,
    boolInputVarSize,
    numInputVarSize,
    pinnedVarSize,
    inputVarSize,
    outputVarSize,
    intermediateVarSize,
    boolVarSize,
    -- Group of variables
    numInputVars,
    inputVars,
    outputVars,
    inputVarsRange,
    boolVarsRange,
    -- For modifying the counters
    bumpNumInputVar,
    bumpBoolInputVar,
    bumpIntermediateVar,
    setOutputVarSize,
    setIntermediateVarSize,
    setNumWidth,
    getNumWidth,
    -- Helper function for pretty printing
    indent,
    ----
    getBitVar,
  )
where

import Control.DeepSeq (NFData)
import Data.Foldable (toList)
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq ((:|>)))
import qualified Data.Sequence as Seq
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Keelung.Types (Var)

-- Current layout of variables
--
-- ┏━━━━━━━━━━━━━━━━━┓
-- ┃         Output  ┃
-- ┣─────────────────┫
-- ┃   Number Input  ┃
-- ┣─────────────────┫─ ─ ─ ─ ─ ─ ─ ─
-- ┃  Boolean Input  ┃               │
-- ┣─────────────────┫   Contiguous chunk of bits
-- ┃  Binary Rep of  ┃
-- ┃   Number Input  ┃               │
-- ┣─────────────────┫─ ─ ─ ─ ─ ─ ─ ─
-- ┃   Intermediate  ┃
-- ┗━━━━━━━━━━━━━━━━━┛
--
--------------------------------------------------------------------------------

-- | Variable bookkeeping
data VarCounters = VarCounters
  { -- Size of output variables
    varOutput :: !Int,
    -- Size of Number input variables
    varNumInput :: !Int,
    -- Size of Boolean input variables
    varBoolInput :: !Int,
    -- Sequence of input variables
    varInputSequence :: !(Seq InputVar),
    -- Width of binary representation of Numbers
    varNumWidth :: !Int,
    -- Size of intermediate variables
    varIntermediate :: !Int
  }
  deriving (Generic, NFData, Eq)

instance Serialize VarCounters

instance Show VarCounters where
  show counters =
    "Total variable size: "
      <> show (totalVarSize counters)
      <> showInputVars
      <> showOutputVars
    where
      showInputVars = case inputVarSize counters of
        0 -> ""
        1 -> "\nInput variables: $0"
        n -> "\nInput variables: $0 .. $" <> show (n - 1)

      showOutputVars = case outputVarSize counters of
        0 -> ""
        1 -> "\nOutput variables: $" <> show (inputVarSize counters)
        n -> "\nOutput variables: $" <> show (inputVarSize counters) <> " .. $" <> show (inputVarSize counters + n - 1)

instance Semigroup VarCounters where
  a <> b =
    VarCounters
      { varBoolInput = varBoolInput a + varBoolInput b,
        varNumInput = varNumInput a + varNumInput b,
        varInputSequence = varInputSequence a <> varInputSequence b,
        varNumWidth = max (varNumWidth a) (varNumWidth b),
        varOutput = varOutput a + varOutput b,
        varIntermediate = varIntermediate a + varIntermediate b
      }

instance Monoid VarCounters where
  mempty = makeVarCounters 0 0 0 0 0 mempty

makeVarCounters :: Int -> Int -> Int -> Int -> Int -> [InputVar] -> VarCounters
makeVarCounters numWidth output numInput boolInput intermediate inputSeq =
  VarCounters
    { varOutput = output,
      varNumInput = numInput,
      varBoolInput = boolInput,
      varInputSequence = Seq.fromList inputSeq,
      varNumWidth = numWidth,
      varIntermediate = intermediate
    }

getInputSequence :: VarCounters -> Seq InputVar
getInputSequence = varInputSequence

--------------------------------------------------------------------------------

data InputVar = NumInput Var | BoolInput Var
  deriving (Generic, NFData, Eq, Show)

instance Serialize InputVar

--------------------------------------------------------------------------------

-- | Total size of all variables
totalVarSize :: VarCounters -> Int
totalVarSize counters = pinnedVarSize counters + varIntermediate counters

-- | Calculate the size of variables that are considered "pinned"
--   i.e. they should not be modified by optimizers
pinnedVarSize :: VarCounters -> Int
pinnedVarSize counters = outputVarSize counters + inputVarSize counters

-- | Size of input variables
--      = size of Number input variables
--      + size of Boolean input variables
--      + size of binary representation of Number input variables
inputVarSize :: VarCounters -> Int
inputVarSize counters = varBoolInput counters + (1 + varNumWidth counters) * varNumInput counters

outputVarSize :: VarCounters -> Int
outputVarSize = varOutput

intermediateVarSize :: VarCounters -> Int
intermediateVarSize = varIntermediate

boolInputVarSize :: VarCounters -> Int
boolInputVarSize = varBoolInput

numInputVarSize :: VarCounters -> Int
numInputVarSize = varNumInput

boolVarSize :: VarCounters -> Int
boolVarSize counters = varBoolInput counters + varNumWidth counters * varNumInput counters

--------------------------------------------------------------------------------

-- | Return the variable index of the bit of a Number input variable
getBitVar :: VarCounters -> Int -> Int -> Maybe Int
getBitVar counters inputVarIndex bitIndex = (+) bitIndex <$> getNumInputIndex
  where
    getNumInputIndex :: Maybe Int
    getNumInputIndex =
      case getInputSequence counters Seq.!? (inputVarIndex - varOutput counters) of
        Just (NumInput n) -> Just $ varOutput counters + varNumInput counters + varBoolInput counters + varNumWidth counters * n
        _ -> Nothing

-- | Return the (mixed) indices of Number input variables
numInputVars :: VarCounters -> [Var]
numInputVars counters = mapMaybe extractNumInput (toList (varInputSequence counters))
  where
    extractNumInput :: InputVar -> Maybe Var
    extractNumInput (NumInput n) = Just (varOutput counters + n)
    extractNumInput _ = Nothing

-- | Return the indices of all input variables
inputVars :: VarCounters -> [Var]
inputVars counters = [outputVarSize counters .. pinnedVarSize counters - 1]

-- | Return the indices of all output variables
outputVars :: VarCounters -> [Var]
outputVars counters = [0 .. outputVarSize counters - 1]


inputVarsRange :: VarCounters -> (Int, Int)
inputVarsRange counters = (varOutput counters, pinnedVarSize counters - 1)

boolVarsRange :: VarCounters -> (Int, Int)
boolVarsRange counters = (varOutput counters + varNumInput counters, pinnedVarSize counters - 1)

--------------------------------------------------------------------------------

-- | Bump the Number input variable counter
bumpNumInputVar :: VarCounters -> VarCounters
bumpNumInputVar counters =
  let var = varNumInput counters
   in counters
        { varNumInput = var + 1,
          varInputSequence = varInputSequence counters :|> NumInput var
        }

-- | Bump the Boolean input variable counter
bumpBoolInputVar :: VarCounters -> VarCounters
bumpBoolInputVar counters =
  let var = varBoolInput counters
   in counters
        { varBoolInput = var + 1,
          varInputSequence = varInputSequence counters :|> BoolInput var
        }

-- | Bump the output variable counter
bumpIntermediateVar :: VarCounters -> VarCounters
bumpIntermediateVar counters = counters {varIntermediate = varIntermediate counters + 1}

setOutputVarSize :: Int -> VarCounters -> VarCounters
setOutputVarSize size counters = counters {varOutput = size}

setIntermediateVarSize :: Int -> VarCounters -> VarCounters
setIntermediateVarSize size counters = counters {varIntermediate = size}

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
