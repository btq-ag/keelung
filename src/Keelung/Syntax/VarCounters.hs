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
    customInputSizeOf,
    totalCustomInputSize,
    pinnedVarSize,
    inputVarSize,
    outputVarSize,
    intermediateVarSize,
    numBinRepVarSize,
    customBinRepVarSize,
    totalBoolVarSize,
    -- Group of variables
    inputVars,
    outputVars,
    inputVarsRange,
    boolVarsRange,
    -- Blended index of variables
    blendedNumInputVars,
    blendedCustomInputVars,
    blendNumInputVar,
    blendCustomInputVar,
    blendBoolInputVar,
    blendIntermediateVar,
    -- For modifying the counters
    bumpNumInputVar,
    bumpBoolInputVar,
    bumpCustomInputVar,
    bumpIntermediateVar,
    setOutputVarSize,
    setIntermediateVarSize,
    setNumBitWidth,
    getNumBitWidth,
    -- Helper function for pretty printing
    indent,
    ----
    getBitVar,
  )
where

import Control.DeepSeq (NFData)
import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Sequence (Seq ((:|>)))
import qualified Data.Sequence as Seq
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Keelung.Types (Var)

-- Current layout of variables
-- ┏━━━━━━━━━━━━━━━━━┓
-- ┃         Output  ┃
-- ┣─────────────────┫
-- ┃   Number Input  ┃
-- ┣─────────────────┫
-- ┃   Custom Input  ┃
-- ┣─────────────────┫─ ─ ─ ─ ─ ─ ─ ─
-- ┃  Boolean Input  ┃               │
-- ┣─────────────────┫
-- ┃  Binary Rep of  ┃
-- ┃   Number Input  ┃   Contiguous chunk of bits
-- ┣─────────────────┫
-- ┃  Binary Rep of  ┃
-- ┃   Custom Input  ┃               │
-- ┣─────────────────┫─ ─ ─ ─ ─ ─ ─ ─
-- ┃   Intermediate  ┃
-- ┗━━━━━━━━━━━━━━━━━┛
--------------------------------------------------------------------------------

-- | Variable bookkeeping
data VarCounters = VarCounters
  { -- Size of output variables
    varOutput :: !Int,
    -- Size of Number input variables
    varNumInput :: !Int,
    -- Size of Boolean input variables
    varBoolInput :: !Int,
    -- Size of custom input variables of different bit width
    varCustomInputs :: !(IntMap Int),
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
      <> showOutputVars
      <> showInputVars
    where
      showOutputVars = case outputVarSize counters of
        0 -> ""
        1 -> "\nOutput variable : $0"
        n -> "\nOutput variables: $0 .. $" <> show (n - 1)

      showInputVars = case inputVarSize counters of
        0 -> ""
        1 -> "\nInput  variable : $" <> show (fst (inputVarsRange counters))
        _ -> "\nInput  variables: $" <> show (fst (inputVarsRange counters)) <> " .. $" <> show (snd (inputVarsRange counters) - 1)

instance Semigroup VarCounters where
  a <> b =
    VarCounters
      { varOutput = varOutput a + varOutput b,
        varNumInput = varNumInput a + varNumInput b,
        varBoolInput = varBoolInput a + varBoolInput b,
        varCustomInputs = varCustomInputs a <> varCustomInputs b,
        varInputSequence = varInputSequence a <> varInputSequence b,
        varNumWidth = max (varNumWidth a) (varNumWidth b),
        varIntermediate = varIntermediate a + varIntermediate b
      }

instance Monoid VarCounters where
  mempty = makeVarCounters 0 0 0 0 0 mempty mempty

makeVarCounters :: Int -> Int -> Int -> Int -> Int -> [InputVar] -> [(Int, Int)] -> VarCounters
makeVarCounters numWidth output numInput boolInput intermediate inputSeq customInputs =
  VarCounters
    { varOutput = output,
      varNumInput = numInput,
      varBoolInput = boolInput,
      varCustomInputs = IntMap.fromList customInputs,
      varInputSequence = Seq.fromList inputSeq,
      varNumWidth = numWidth,
      varIntermediate = intermediate
    }

getInputSequence :: VarCounters -> Seq InputVar
getInputSequence = varInputSequence

--------------------------------------------------------------------------------

data InputVar = NumInput Var | BoolInput Var | CustomInput Int Var
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
--      + size of Custom input variables
--      + size of Boolean input variables
--      + size of binary representation of Number input variables
--      + size of binary representation of Custom input variables
inputVarSize :: VarCounters -> Int
inputVarSize counters =
  varBoolInput counters
    + (1 + varNumWidth counters) * varNumInput counters
    + totalCustomInputSize counters
    + customBinRepVarSize counters

outputVarSize :: VarCounters -> Int
outputVarSize = varOutput

-- | Size of custom input variables of certain bit width
customInputSizeOf :: Int -> VarCounters -> Int
customInputSizeOf width counters = fromMaybe 0 (IntMap.lookup width (varCustomInputs counters))

-- | Size of custom input variables of all bit width combined
totalCustomInputSize :: VarCounters -> Int
totalCustomInputSize counters = IntMap.size (varCustomInputs counters)

intermediateVarSize :: VarCounters -> Int
intermediateVarSize = varIntermediate

boolInputVarSize :: VarCounters -> Int
boolInputVarSize = varBoolInput

numInputVarSize :: VarCounters -> Int
numInputVarSize = varNumInput

numBinRepVarSize :: VarCounters -> Int
numBinRepVarSize counters = varNumWidth counters * varNumInput counters

customBinRepVarSize :: VarCounters -> Int
customBinRepVarSize counters =
  IntMap.foldlWithKey'
    (\acc width size -> acc + width * size)
    0
    (varCustomInputs counters)

-- | Size of all Boolean variables (for imposing the Boolean constraint)
totalBoolVarSize :: VarCounters -> Int
totalBoolVarSize counters = varBoolInput counters + varNumWidth counters * varNumInput counters + customBinRepVarSize counters

--------------------------------------------------------------------------------

-- | Return the indices of all input variables
inputVars :: VarCounters -> [Var]
inputVars counters = [outputVarSize counters .. pinnedVarSize counters - 1]

-- | Return the indices of all output variables
outputVars :: VarCounters -> [Var]
outputVars counters = [0 .. outputVarSize counters - 1]

inputVarsRange :: VarCounters -> (Int, Int)
inputVarsRange counters = (varOutput counters, pinnedVarSize counters)

boolVarsRange :: VarCounters -> (Int, Int)
boolVarsRange counters = (varOutput counters + varNumInput counters + totalCustomInputSize counters, pinnedVarSize counters)

--------------------------------------------------------------------------------

-- | Return the variable index of the bit of a Number input variable
getBitVar :: VarCounters -> Int -> Int -> Maybe Int
getBitVar counters blendedIndex bitIndex = (+) bitIndex <$> getIndex
  where
    getIndex :: Maybe Int
    getIndex =
      case getInputSequence counters Seq.!? (blendedIndex - varOutput counters) of
        Just (NumInput n) ->
          Just $
            outputVarSize counters
              + numInputVarSize counters
              + totalCustomInputSize counters
              + boolInputVarSize counters
              + varNumWidth counters * n
        Just (CustomInput width n) ->
          Just $
            outputVarSize counters
              + numInputVarSize counters
              + totalCustomInputSize counters
              + boolInputVarSize counters
              + numBinRepVarSize counters
              -- size of all custom inputs of width smaller than this one
              + IntMap.foldlWithKey'
                (\acc w size -> acc + w * size)
                0
                (IntMap.filterWithKey (\w _ -> w < width) (varCustomInputs counters))
              + width * n
        _ -> Nothing

-- | Return the blended indices of all Number input variables
blendedNumInputVars :: VarCounters -> [Var]
blendedNumInputVars counters = mapMaybe extractNumInput (zip [varOutput counters ..] (toList (varInputSequence counters)))
  where
    extractNumInput :: (Int, InputVar) -> Maybe Var
    extractNumInput (index, NumInput _) = Just index
    extractNumInput _ = Nothing

-- | Return the blended indices of all Custom input variables along with their bit width
--    [(bitWidth, blendedIndices)]
blendedCustomInputVars :: VarCounters -> IntMap IntSet
blendedCustomInputVars counters =
  IntMap.foldlWithKey'
    ( \acc index inputVar -> case inputVar of
        CustomInput width _ -> IntMap.insertWith (<>) width (IntSet.singleton index) acc
        _ -> acc
    )
    mempty
    (IntMap.fromList (zip [varOutput counters ..] (toList (varInputSequence counters))))

-- | Return the blended index of a Number input variable
blendNumInputVar :: VarCounters -> Int -> Int
blendNumInputVar counters index = index + varOutput counters

-- | Return the blended index of a Custom input variable
blendCustomInputVar :: VarCounters -> Int -> Int
blendCustomInputVar counters index = index + varOutput counters + varNumInput counters

-- | Return the blended index of a Boolean input variable
blendBoolInputVar :: VarCounters -> Int -> Int
blendBoolInputVar counters index = index + varOutput counters + varNumInput counters + totalCustomInputSize counters

-- | Return the blended index of an intermediate variable
blendIntermediateVar :: VarCounters -> Int -> Int
blendIntermediateVar counters index = index + pinnedVarSize counters

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

-- | Bump a custom input variable counter of some bit width
bumpCustomInputVar :: Int -> VarCounters -> VarCounters
bumpCustomInputVar width counters =
  let (result, after) = IntMap.insertLookupWithKey f width 1 (varCustomInputs counters)
   in counters
        { varCustomInputs = after,
          varInputSequence = varInputSequence counters :|> CustomInput width (fromMaybe 0 result)
        }
  where
    -- bump the counter if and only if it exists
    f _key _new old = old + 1

-- | Bump the output variable counter
bumpIntermediateVar :: VarCounters -> VarCounters
bumpIntermediateVar counters = counters {varIntermediate = varIntermediate counters + 1}

setOutputVarSize :: Int -> VarCounters -> VarCounters
setOutputVarSize size counters = counters {varOutput = size}

setIntermediateVarSize :: Int -> VarCounters -> VarCounters
setIntermediateVarSize size counters = counters {varIntermediate = size}

-- | Updates the width of binary representation of a Number when it's known
setNumBitWidth :: Int -> VarCounters -> VarCounters
setNumBitWidth width counters = counters {varNumWidth = width}

-- | Get the width of binary representation of a Number
getNumBitWidth :: VarCounters -> Int
getNumBitWidth = varNumWidth

--------------------------------------------------------------------------------

-- | Handy function for prettifying VarCounters
indent :: String -> String
indent = unlines . map ("  " <>) . lines