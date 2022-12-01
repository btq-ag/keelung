{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Keelung.Syntax.Counters
  ( Counters (Counters),
    SmallCounters (SmallCounters),
    VarType (..),
    VarSort (..),
    reindex,
    getCount,
    getCountBySort,
    getCountByType,
    getTotalCount,
    addCount,
    -- for constraint generation
    getOutputVarRange,
    getOutputBinRepRange,
    getInputVarRange,
    getBinRepConstraintSize,
    getBinReps,
    getBooleanConstraintSize,
    getBooleanConstraintRanges,
    -- for parsing raw inputs
    getInputSequence,
    -- workaround for variable renumbering
    setReducedCount,
    -- for pretty printing
    prettyConstraints,
    prettyVariables,
    prettyBooleanConstraints,
    prettyBinRepConstraints,
  )
where

import Control.DeepSeq (NFData)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Keelung.Syntax.BinRep (BinRep (..))

------------------------------------------------------------------------------

type Var = Int

type Width = Int

-- | "Types" of variables.
data VarType = OfField | OfBoolean | OfUIntBinRep Width | OfUInt Width
  deriving (Generic, NFData, Eq, Show)

instance Serialize VarType

-- | "Sorts" of variables.
data VarSort = OfOutput | OfInput | OfIntermediate

------------------------------------------------------------------------------

data SmallCounters = SmallCounters
  { sizeF :: !Int, -- size of field element variables
    sizeB :: !Int, -- size of Boolean variables
    sizeUBinReps :: !(IntMap Int), -- size of binary representations of unsigned integers
    sizeU :: !(IntMap Int) -- size of unsigned integer variables
  }
  deriving (Generic, NFData, Eq, Show)

instance Serialize SmallCounters

instance Semigroup SmallCounters where
  SmallCounters sF1 sB1 sUBinReps1 sU1 <> SmallCounters sF2 sB2 sUBinReps2 sU2 =
    SmallCounters
      (sF1 + sF2)
      (sB1 + sB2)
      (IntMap.unionWith (+) sUBinReps1 sUBinReps2)
      (IntMap.unionWith (+) sU1 sU2)

instance Monoid SmallCounters where
  mempty = SmallCounters 0 0 IntMap.empty IntMap.empty

binRepSize :: IntMap Int -> Int
binRepSize = IntMap.foldlWithKey' (\acc width size -> acc + width * size) 0

uIntSize :: IntMap Int -> Int
uIntSize = sum

smallCounterSize :: SmallCounters -> Int
smallCounterSize (SmallCounters f b ubr u) =
  f + b + binRepSize ubr + uIntSize u

--------------------------------------------------------------------------------

data Counters = Counters
  { countOutput :: !SmallCounters, -- counters for output variables
    countInput :: !SmallCounters, -- counters for input variables
    countIntermediate :: !SmallCounters, -- counters for intermediate variables
    countInputSequence :: !(Seq (VarType, Int)), -- Sequence of input variables
    countReducedVarHack :: !Int -- HACK, keep track of the number of variables reduced after renumbering
  }
  deriving (Generic, NFData, Eq, Show)

instance Serialize Counters

instance Semigroup Counters where
  Counters cOut1 cIn1 cInt1 cInSeq1 cRed1 <> Counters cOut2 cIn2 cInt2 cInSeq2 cRed2 =
    Counters
      (cOut1 <> cOut2)
      (cIn1 <> cIn2)
      (cInt1 <> cInt2)
      (cInSeq1 <> cInSeq2)
      (cRed1 + cRed2)

instance Monoid Counters where
  mempty = Counters mempty mempty mempty mempty 0

--------------------------------------------------------------------------------

-- | Get the current count for a variable of the given type and sort.
getCount :: VarSort -> VarType -> Counters -> Int
getCount sort typ (Counters o i x _ _) =
  case sort of
    OfOutput -> go o
    OfInput -> go i
    OfIntermediate -> go x
  where
    go :: SmallCounters -> Int
    go (SmallCounters f b ubr u) =
      case typ of
        OfField -> f
        OfBoolean -> b
        OfUIntBinRep w -> w * IntMap.findWithDefault 0 w ubr
        OfUInt w -> IntMap.findWithDefault 0 w u

-- | Get the current count for a variable group of the given sort.
getCountBySort :: VarSort -> Counters -> Int
getCountBySort sort (Counters o i x _ _) =
  case sort of
    OfOutput -> smallCounterSize o
    OfInput -> smallCounterSize i
    OfIntermediate -> smallCounterSize x

-- | Get the current count for a variable group of the given type.
getCountByType :: VarType -> Counters -> Int
getCountByType typ (Counters o i x _ _) =
  case typ of
    OfField -> sizeF o + sizeF i + sizeF x
    OfBoolean -> sizeB o + sizeB i + sizeB x
    OfUIntBinRep _ -> binRepSize (sizeUBinReps o) + binRepSize (sizeUBinReps i) + binRepSize (sizeUBinReps x)
    OfUInt _ -> uIntSize (sizeU o) + uIntSize (sizeU i) + uIntSize (sizeU x)

setReducedCount :: Int -> Counters -> Counters
setReducedCount n (Counters o i x s _) = Counters o i x s n

-- | Total count of variables
getTotalCount :: Counters -> Int
getTotalCount (Counters o i x _ reduced) =
  -- 'countReducedVarHack' should only have effect on intermediate variables
  (smallCounterSize o + smallCounterSize i) + (0 `max` (smallCounterSize x - reduced))

-- | Set the current count for a variable of the given type and sort.
addCount :: VarSort -> VarType -> Int -> Counters -> Counters
addCount sort typ n (Counters o i x is r) =
  case sort of
    OfOutput -> Counters (adjustSmallCounters o) i x is r
    OfInput -> Counters o (adjustSmallCounters i) x (is <> newInputSequence) r
    OfIntermediate -> Counters o i (adjustSmallCounters x) is r
  where
    adjustSmallCounters :: SmallCounters -> SmallCounters
    adjustSmallCounters (SmallCounters f b ubr u) =
      case typ of
        OfField -> SmallCounters (f + n) b ubr u
        OfBoolean -> SmallCounters f (b + n) ubr u
        OfUIntBinRep _ -> error "[ panic ] Should use `OfUInt` to adjust the counter instead"
        OfUInt w -> SmallCounters f b (IntMap.insertWith (+) w n ubr) (IntMap.insertWith (+) w n u)

    oldCount = getCount sort typ (Counters o i x is r)

    newInputSequence :: Seq (VarType, Int)
    newInputSequence = Seq.fromList [(typ, index) | index <- [oldCount .. oldCount + n - 1]]

-- | For parsing raw inputs
getInputSequence :: Counters -> Seq (VarType, Int)
getInputSequence = countInputSequence

--------------------------------------------------------------------------------

-- | Re-index variables of different sorts and types
reindex :: Counters -> VarSort -> VarType -> Var -> Var
reindex counters sort typ index = offsetOfSort counters sort + offsetOfType (choose sort counters) typ index
  where
    choose :: VarSort -> Counters -> SmallCounters
    choose OfOutput = countOutput
    choose OfInput = countInput
    choose OfIntermediate = countIntermediate

offsetOfSort :: Counters -> VarSort -> Int
offsetOfSort _ OfOutput = 0
offsetOfSort counters OfInput = smallCounterSize (countOutput counters)
offsetOfSort counters OfIntermediate = smallCounterSize (countOutput counters) + smallCounterSize (countInput counters)

offsetOfType :: SmallCounters -> VarType -> Int -> Int
offsetOfType _ OfField index = index
offsetOfType (SmallCounters f _ _ _) OfBoolean index = f + index
offsetOfType (SmallCounters f b ubr _) (OfUIntBinRep width) index =
  f + b
    + IntMap.size (IntMap.filterWithKey (\width' _ -> width' < width) ubr)
    + width * index
offsetOfType (SmallCounters f b ubr u) (OfUInt width) index = f + b + binRepSize ubr + IntMap.size (IntMap.filterWithKey (\width' _ -> width' < width) u) + index

--------------------------------------------------------------------------------

getOutputVarRange :: Counters -> (Int, Int)
getOutputVarRange counters = (offsetOfSort counters OfOutput, offsetOfSort counters OfInput)

getOutputBinRepRange :: Counters -> (Int, Int)
getOutputBinRepRange counters =
  let start = offsetOfSort counters OfOutput + getCount OfOutput OfField counters + getCount OfOutput OfBoolean counters
      size = binRepSize (sizeUBinReps (countOutput counters))
   in (start, start + size)

getInputVarRange :: Counters -> (Int, Int)
getInputVarRange counters =
  let inputOffset = offsetOfSort counters OfInput
      inputSize = getCountBySort OfInput counters
   in (inputOffset, inputOffset + inputSize)

-- | Generate one BinRep constraint for each UInt input & output variable
getBinRepConstraintSize :: Counters -> Int
getBinRepConstraintSize (Counters o i _ _ _) = f o + f i
  where
    f (SmallCounters _ _ _ u) = uIntSize u

getBinReps :: Counters -> [BinRep]
getBinReps counters@(Counters o i x _ _) =
  fromSmallCounter OfOutput o ++ fromSmallCounter OfInput i ++ fromSmallCounter OfIntermediate x
  where
    fromSmallCounter :: VarSort -> SmallCounters -> [BinRep]
    fromSmallCounter sort (SmallCounters _ _ ubr _) = concatMap (fromPair sort) (IntMap.toList ubr)

    fromPair :: VarSort -> (Width, Int) -> [BinRep]
    fromPair sort (width, count) =
      let varOffset = reindex counters sort (OfUInt width) 0
          binRepOffset = reindex counters sort (OfUIntBinRep width) 0
       in [BinRep (varOffset + index) width (binRepOffset + width * index) 0 | index <- [0 .. count - 1]]

-- | Variables that needed to be constrained to be Boolean
--    1. Boolean output variables
--    2. UInt BinReps output variables
--    3. Boolean input variables
--    4. UInt BinReps input variables
getBooleanConstraintSize :: Counters -> Int
getBooleanConstraintSize (Counters o i _ _ _) = f o + f i
  where
    f (SmallCounters _ b ubr _) = b + binRepSize ubr

-- | Variables that needed to be constrained to be Boolean
--    1. Boolean output variables
--    2. UInt BinReps output variables
--    3. Boolean input variables
--    4. UInt BinReps input variables
getBooleanConstraintRanges :: Counters -> [(Int, Int)]
getBooleanConstraintRanges counters@(Counters o i _ _ _) =
  mergeSegments [booleanVarRange OfOutput o, booleanVarRange OfInput i]
  where
    booleanVarRange :: VarSort -> SmallCounters -> (Int, Int)
    booleanVarRange sort (SmallCounters _ b ubr _) = (reindex counters sort OfBoolean 0, reindex counters sort OfBoolean 0 + b + binRepSize ubr)

    mergeSegments :: [(Int, Int)] -> [(Int, Int)]
    mergeSegments [] = []
    mergeSegments [(start, end)]
      | end == start = []
      | otherwise = [(start, end)]
    mergeSegments ((start, end) : (start', end') : xs)
      | end == start = mergeSegments ((start', end') : xs)
      | end == start' = mergeSegments ((start, end') : xs)
      | otherwise = (start, end) : mergeSegments ((start', end') : xs)

--------------------------------------------------------------------------------

prettyVariables :: Counters -> String
prettyVariables counters@(Counters o i _ _ _) =
  let inputOffset = offsetOfSort counters OfInput
      outputOffset = offsetOfSort counters OfOutput
      totalSize = getTotalCount counters

      inputVars = case smallCounterSize o of
        0 -> ""
        1 -> "    Output variable : $" <> show outputOffset <> "\n"
        n -> "    Output variables: $" <> show outputOffset <> " ... $" <> show (outputOffset + n - 1) <> "\n"
      ouputVars = case smallCounterSize i of
        0 -> ""
        1 -> "    Input  variable  : $" <> show inputOffset <> "\n"
        n -> "    Input  variables : $" <> show inputOffset <> " ... $" <> show (inputOffset + n - 1) <> "\n"
   in if totalSize == 0
        then ""
        else
          "  Variables (" <> show totalSize <> "):\n\n"
            <> ouputVars
            <> inputVars
            <> "\n"

prettyConstraints :: Show constraint => Counters -> [constraint] -> String
prettyConstraints counters cs =
  showConstraintSummary
    <> showOrdinaryConstraints
    <> showBooleanConstraints
    <> showBinRepConstraints
  where
    -- sizes of constraint groups
    totalBinRepConstraintSize = getBinRepConstraintSize counters
    booleanConstraintSize = getBooleanConstraintSize counters
    ordinaryConstraintSize = length cs

    -- summary of constraint groups
    showConstraintSummary =
      "  Constriant ("
        <> show (ordinaryConstraintSize + booleanConstraintSize + totalBinRepConstraintSize)
        <> "): \n"

    -- Ordinary constraints
    showOrdinaryConstraints =
      if ordinaryConstraintSize == 0
        then ""
        else
          "    Ordinary constriants (" <> show ordinaryConstraintSize <> "):\n\n"
            <> unlines (map (\x -> "      " <> show x) cs)
            <> "\n"

    -- Boolean constraints
    showBooleanConstraints =
      if booleanConstraintSize == 0
        then ""
        else
          "    Boolean constriants (" <> show booleanConstraintSize <> "):\n\n"
            <> unlines (map ("      " <>) (prettyBooleanConstraints counters))
            <> "\n"

    -- BinRep constraints
    showBinRepConstraints =
      if totalBinRepConstraintSize == 0
        then ""
        else
          "    Binary representation constriants (" <> show totalBinRepConstraintSize <> "):\n\n"
            <> unlines (map ("      " <>) (prettyBinRepConstraints counters))
            <> "\n"

prettyBooleanConstraints :: Counters -> [String]
prettyBooleanConstraints counters =
  concatMap showSegment (getBooleanConstraintRanges counters)
  where
    showSegment :: (Int, Int) -> [String]
    showSegment (start, end) =
      case end - start of
        0 -> []
        1 -> [showBooleanConstraint start]
        2 ->
          [ showBooleanConstraint start,
            showBooleanConstraint (start + 1)
          ]
        3 ->
          [ showBooleanConstraint start,
            showBooleanConstraint (start + 1),
            showBooleanConstraint (start + 2)
          ]
        _ ->
          [ showBooleanConstraint start,
            "  ...",
            showBooleanConstraint (end - 1)
          ]

    showBooleanConstraint :: Int -> String
    showBooleanConstraint n = "$" <> show n <> " = $" <> show n <> " * $" <> show n

prettyBinRepConstraints :: Counters -> [String]
prettyBinRepConstraints = map show . getBinReps
