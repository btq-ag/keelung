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
    numInputVarsRange,
    customInputVarsTotalRange,
    customInputVarsRanges,
    -- Blended index of variables
    blendedCustomInputVarSizes,
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
    lookupBinRepStart,
  )
where

import Control.DeepSeq (NFData)
import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (fromMaybe)
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
totalCustomInputSize counters = IntMap.foldl' (+) 0 (varCustomInputs counters)

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

numInputVarsRange :: VarCounters -> (Int, Int)
numInputVarsRange counters = (varOutput counters, varOutput counters + varNumInput counters)

customInputVarsTotalRange :: VarCounters -> (Int, Int)
customInputVarsTotalRange counters = (varOutput counters + varNumInput counters, varOutput counters + varNumInput counters + totalCustomInputSize counters)

customInputVarsRanges :: VarCounters -> IntMap (Int, Int)
customInputVarsRanges counters =
  let startIndex = varOutput counters + varNumInput counters
   in snd $ IntMap.mapAccumWithKey (\acc _ size -> (acc + size, (acc, acc + size))) startIndex (varCustomInputs counters)

boolVarsRange :: VarCounters -> (Int, Int)
boolVarsRange counters = (varOutput counters + varNumInput counters + totalCustomInputSize counters, pinnedVarSize counters)

--------------------------------------------------------------------------------

-- | Blended index -> bin rep index
lookupBinRepStart :: VarCounters -> Var -> Maybe Var
lookupBinRepStart counters var
  -- outputs 
  | var < varOutput counters = Nothing
  -- number inputs
  | var < varOutput counters + varNumInput counters =
    let nth = var - varOutput counters
     in Just $
          outputVarSize counters
            + numInputVarSize counters
            + totalCustomInputSize counters
            + boolInputVarSize counters
            + varNumWidth counters * nth
  -- custom inputs
  | var < varOutput counters + varNumInput counters + totalCustomInputSize counters =
    let nth = var - (varOutput counters + varNumInput counters)
        offset = 
          outputVarSize counters
            + numInputVarSize counters
            + totalCustomInputSize counters
            + boolInputVarSize counters
            + numBinRepVarSize counters
     in case lookupCustomInputBinRepStart (convertCustomInputMap (varCustomInputs counters)) nth of
          Nothing -> Nothing
          Just index -> Just (offset + index)
  | otherwise = Nothing
  where
    -- objective: convert CustomInputMap like [(3, 1), (4, 2), (5, 1)] into [(0, (3, 0)), (1, (4, 3)), (3, (5, 11))]
    convertCustomInputMap :: IntMap Int -> IntMap (Int, Int)
    convertCustomInputMap =
      snd
        . IntMap.foldlWithKey'
          (\((accKey, accVal), acc) width size -> ((accKey + size, accVal + width * size), IntMap.insert accKey (width, accVal) acc))
          ((0, 0), mempty)

    lookupCustomInputBinRepStart :: IntMap (Int, Int) -> Int -> Maybe Int
    lookupCustomInputBinRepStart xs index = IntMap.lookupLE index xs >>= \(key, (width, offset)) -> Just $ offset + width * (index - key)

-- | Return the blended indices of all Custom input variables along with their bit width
--    [(bitWidth, blendedIndices)]
blendedCustomInputVarSizes :: VarCounters -> IntMap Int
blendedCustomInputVarSizes counters =
  IntMap.foldlWithKey'
    ( \acc _ inputVar -> case inputVar of
        CustomInput width _ -> IntMap.insertWith (+) width 1 acc
        _ -> acc
    )
    mempty
    (IntMap.fromList (zip [varOutput counters ..] (toList (varInputSequence counters))))

-- | Return the blended index of a Number input variable
blendNumInputVar :: VarCounters -> Int -> Int
blendNumInputVar counters index = index + varOutput counters

-- | Return the blended index of a Custom input variable
blendCustomInputVar :: VarCounters -> Int -> Int -> Int
blendCustomInputVar counters width index =
  let offset = varOutput counters + varNumInput counters
      smallerEntries = IntMap.filterWithKey (\width' _ -> width' < width) (varCustomInputs counters)
   in offset + IntMap.size smallerEntries + index

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