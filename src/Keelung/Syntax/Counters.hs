{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_HADDOCK hide #-}

module Keelung.Syntax.Counters
  ( Counters (..),
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
    getPublicInputVarRange,
    getPrivateInputVarRange,
    getBinRepConstraintSize,
    getBinReps,
    getBooleanConstraintSize,
    getBooleanConstraintRanges,
    -- for parsing raw inputs
    getPublicInputSequence,
    getPrivateInputSequence,
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
import Data.IntMap.Strict qualified as IntMap
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Keelung.Data.BinRep (BinRep (..))
import Keelung.Data.Struct (Struct (..))

------------------------------------------------------------------------------

type Var = Int

type Width = Int

-- | "Types" of variables.
data VarType = OfField | OfBoolean | OfUIntBinRep Width | OfUInt Width
  deriving (Generic, NFData, Eq, Show)

instance Serialize VarType

-- | "Sorts" of variables.
data VarSort = OfOutput | OfPublicInput | OfPrivateInput | OfIntermediate

------------------------------------------------------------------------------

type SmallCounters = Struct Int Int Int

binRepSize :: IntMap Int -> Int
binRepSize = IntMap.foldlWithKey' (\acc width size -> acc + width * size) 0

uIntSize :: IntMap Int -> Int
uIntSize = sum

smallCounterSize :: SmallCounters -> Int
smallCounterSize (Struct f b u) =
  f + b + binRepSize u + uIntSize u

--------------------------------------------------------------------------------

data Counters = Counters
  { countOutput :: !SmallCounters, -- counters for output variables
    countPublicInput :: !SmallCounters, -- counters for input variables
    countPrivateInput :: !SmallCounters, -- counters for input variables
    countIntermediate :: !SmallCounters, -- counters for intermediate variables
    countPublicInputSequence :: !(Seq VarType), -- Sequence of public input variables
    countPrivateInputSequence :: !(Seq VarType), -- Sequence of private input variables
    countReducedVarHack :: !Int -- HACK, keep track of the number of variables reduced after renumbering
  }
  deriving (Generic, NFData, Eq, Show)

instance Serialize Counters

instance Semigroup Counters where
  Counters cOut1 cPubIn1 cPrivIn1 cInt1 cPubInSeq1 cPrivInSeq1 cRed1 <> Counters cOut2 cPubIn2 cPrivIn2 cInt2 cPubInSeq2 cPrivInSeq2 cRed2 =
    Counters
      (addSmallCounters cOut1 cOut2)
      (addSmallCounters cPubIn1 cPubIn2)
      (addSmallCounters cPrivIn1 cPrivIn2)
      (addSmallCounters cInt1 cInt2)
      (cPubInSeq1 <> cPubInSeq2)
      (cPrivInSeq1 <> cPrivInSeq2)
      (cRed1 + cRed2)
    where
      addSmallCounters :: SmallCounters -> SmallCounters -> SmallCounters
      addSmallCounters (Struct f1 b1 u1) (Struct f2 b2 u2) =
        Struct (f1 + f2) (b1 + b2) (IntMap.unionWith (+) u1 u2)

instance Monoid Counters where
  mempty = Counters (Struct 0 0 mempty) (Struct 0 0 mempty) (Struct 0 0 mempty) (Struct 0 0 mempty) mempty mempty 0

--------------------------------------------------------------------------------

-- | Get the current count for a variable of the given type and sort.
getCount :: VarSort -> VarType -> Counters -> Int
getCount sort typ (Counters o i1 i2 x _ _ _) =
  case sort of
    OfOutput -> go o
    OfPublicInput -> go i1
    OfPrivateInput -> go i2
    OfIntermediate -> go x
  where
    go :: SmallCounters -> Int
    go (Struct f b u) =
      case typ of
        OfField -> f
        OfBoolean -> b
        OfUIntBinRep w -> w * IntMap.findWithDefault 0 w u
        OfUInt w -> IntMap.findWithDefault 0 w u

-- | Get the current count for a variable group of the given sort.
getCountBySort :: VarSort -> Counters -> Int
getCountBySort sort (Counters o i1 i2 x _ _ _) =
  case sort of
    OfOutput -> smallCounterSize o
    OfPublicInput -> smallCounterSize i1
    OfPrivateInput -> smallCounterSize i2
    OfIntermediate -> smallCounterSize x

-- | Get the current count for a variable group of the given type.
getCountByType :: VarType -> Counters -> Int
getCountByType typ (Counters o i1 i2 x _ _ _) =
  case typ of
    OfField -> structF o + structF i1 + structF i2 + structF x
    OfBoolean -> structB o + structB i1 + structB i2 + structB x
    OfUIntBinRep _ -> binRepSize (structU o) + binRepSize (structU i1) + binRepSize (structU i2) + binRepSize (structU x)
    OfUInt _ -> uIntSize (structU o) + uIntSize (structU i1) + uIntSize (structU i2) + uIntSize (structU x)

setReducedCount :: Int -> Counters -> Counters
setReducedCount n (Counters o i1 i2 x s1 s2 _) = Counters o i1 i2 x s1 s2 n

-- | Total count of variables
getTotalCount :: Counters -> Int
getTotalCount (Counters o i1 i2 x _ _ reduced) =
  -- 'countReducedVarHack' should only have effect on intermediate variables
  (smallCounterSize o + smallCounterSize i1 + smallCounterSize i2) + (0 `max` (smallCounterSize x - reduced))

-- | Set the current count for a variable of the given type and sort.
addCount :: VarSort -> VarType -> Int -> Counters -> Counters
addCount sort typ n (Counters o i1 i2 x s1 s2 r) =
  case sort of
    OfOutput -> Counters (adjustSmallCounters o) i1 i2 x s1 s2 r
    OfPublicInput -> Counters o (adjustSmallCounters i1) i2 x (s1 <> newInputSequence) s2 r
    OfPrivateInput -> Counters o i1 (adjustSmallCounters i2) x s1 (s2 <> newInputSequence) r
    OfIntermediate -> Counters o i1 i2 (adjustSmallCounters x) s1 s2 r
  where
    adjustSmallCounters :: SmallCounters -> SmallCounters
    adjustSmallCounters (Struct f b u) =
      case typ of
        OfField -> Struct (f + n) b u
        OfBoolean -> Struct f (b + n) u
        OfUIntBinRep _ -> error "[ panic ] Should use `OfUInt` to adjust the counter instead"
        OfUInt w -> Struct f b (IntMap.insertWith (+) w n u)

    newInputSequence :: Seq VarType
    newInputSequence = Seq.fromList $ replicate n typ

-- | For parsing raw inputs
getPublicInputSequence :: Counters -> Seq VarType
getPublicInputSequence = countPublicInputSequence

getPrivateInputSequence :: Counters -> Seq VarType
getPrivateInputSequence = countPrivateInputSequence

--------------------------------------------------------------------------------

-- | Re-index variables of different sorts and types
reindex :: Counters -> VarSort -> VarType -> Var -> Var
reindex counters sort typ index = offsetOfSort counters sort + offsetOfType (choose sort counters) typ index
  where
    choose :: VarSort -> Counters -> SmallCounters
    choose OfOutput = countOutput
    choose OfPublicInput = countPublicInput
    choose OfPrivateInput = countPrivateInput
    choose OfIntermediate = countIntermediate

offsetOfSort :: Counters -> VarSort -> Int
offsetOfSort _ OfOutput = 0
offsetOfSort counters OfPublicInput = smallCounterSize (countOutput counters)
offsetOfSort counters OfPrivateInput = smallCounterSize (countOutput counters) + smallCounterSize (countPublicInput counters)
offsetOfSort counters OfIntermediate = smallCounterSize (countOutput counters) + smallCounterSize (countPublicInput counters) + smallCounterSize (countPrivateInput counters)

offsetOfType :: SmallCounters -> VarType -> Int -> Int
offsetOfType _ OfField index = index
offsetOfType (Struct f _ _) OfBoolean index = f + index
offsetOfType (Struct f b u) (OfUIntBinRep width) index =
  f
    + b
    + sum
      ( IntMap.mapWithKey
          ( \width' count -> case compare width width' of
              LT -> 0
              EQ -> index * width
              GT -> width' * count
          )
          u
      )
-- + IntMap.size (IntMap.filterWithKey (\width' _ -> width' < width) u)
offsetOfType (Struct f b u) (OfUInt width) index = f + b + binRepSize u + IntMap.size (IntMap.filterWithKey (\width' _ -> width' < width) u) + index

--------------------------------------------------------------------------------

getOutputVarRange :: Counters -> (Int, Int)
getOutputVarRange counters = (offsetOfSort counters OfOutput, offsetOfSort counters OfPublicInput)

getOutputBinRepRange :: Counters -> (Int, Int)
getOutputBinRepRange counters =
  let start = offsetOfSort counters OfOutput + getCount OfOutput OfField counters + getCount OfOutput OfBoolean counters
      size = binRepSize (structU (countOutput counters))
   in (start, start + size)

getPublicInputVarRange :: Counters -> (Int, Int)
getPublicInputVarRange counters =
  let inputOffset = offsetOfSort counters OfPublicInput
      inputSize = getCountBySort OfPublicInput counters
   in (inputOffset, inputOffset + inputSize)

getPrivateInputVarRange :: Counters -> (Int, Int)
getPrivateInputVarRange counters =
  let inputOffset = offsetOfSort counters OfPrivateInput
      inputSize = getCountBySort OfPrivateInput counters
   in (inputOffset, inputOffset + inputSize)

-- | Generate one BinRep constraint for each UInt input & output variable
getBinRepConstraintSize :: Counters -> Int
getBinRepConstraintSize (Counters o i1 i2 _ _ _ _) = f o + f i1 + f i2
  where
    f (Struct _ _ u) = uIntSize u

getBinReps :: Counters -> [BinRep]
getBinReps counters@(Counters o i1 i2 x _ _ _) =
  fromSmallCounter OfOutput o ++ fromSmallCounter OfPublicInput i1 ++ fromSmallCounter OfPrivateInput i2 ++ fromSmallCounter OfIntermediate x
  where
    fromSmallCounter :: VarSort -> SmallCounters -> [BinRep]
    fromSmallCounter sort (Struct _ _ u) = concatMap (fromPair sort) (IntMap.toList u)

    fromPair :: VarSort -> (Width, Int) -> [BinRep]
    fromPair sort (width, count) =
      let varOffset = reindex counters sort (OfUInt width) 0
          binRepOffset = reindex counters sort (OfUIntBinRep width) 0
       in [BinRep (varOffset + index) width (binRepOffset + width * index) | index <- [0 .. count - 1]]

-- | Variables that needed to be constrained to be Boolean
--    1. Boolean output variables
--    2. UInt BinReps output variables
--    3. Boolean input variables
--    4. UInt BinReps input variables
getBooleanConstraintSize :: Counters -> Int
getBooleanConstraintSize (Counters o i1 i2 _ _ _ _) = f o + f i1 + f i2
  where
    f (Struct _ b u) = b + binRepSize u

-- | Variables that needed to be constrained to be Boolean
--    1. Boolean output variables
--    2. UInt BinReps output variables
--    3. Boolean input variables
--    4. UInt BinReps input variables
getBooleanConstraintRanges :: Counters -> [(Int, Int)]
getBooleanConstraintRanges counters@(Counters o i1 i2 _ _ _ _) =
  mergeSegments [booleanVarRange OfOutput o, booleanVarRange OfPublicInput i1, booleanVarRange OfPrivateInput i2]
  where
    booleanVarRange :: VarSort -> SmallCounters -> (Int, Int)
    booleanVarRange sort (Struct _ b u) = (reindex counters sort OfBoolean 0, reindex counters sort OfBoolean 0 + b + binRepSize u)

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
prettyVariables counters@(Counters o i1 i2 _ _ _ _) =
  let publicInputOffset = offsetOfSort counters OfPublicInput
      privateInputOffset = offsetOfSort counters OfPrivateInput
      outputOffset = offsetOfSort counters OfOutput
      totalSize = getTotalCount counters

      outputVars = case smallCounterSize o of
        0 -> ""
        1 -> "    Output variable : $" <> show outputOffset <> "\n"
        n -> "    Output variables: $" <> show outputOffset <> " ... $" <> show (outputOffset + n - 1) <> "\n"
      publicInputVars = case smallCounterSize i1 of
        0 -> ""
        1 -> "    Public Input variable : $" <> show publicInputOffset <> "\n"
        n -> "    Public Input variables: $" <> show publicInputOffset <> " ... $" <> show (publicInputOffset + n - 1) <> "\n"
      privateInputVars = case smallCounterSize i2 of
        0 -> ""
        1 -> "    Private Input variable : $" <> show privateInputOffset <> "\n"
        n -> "    Private Input variables: $" <> show privateInputOffset <> " ... $" <> show (privateInputOffset + n - 1) <> "\n"
   in if totalSize == 0
        then ""
        else
          "  Variables ("
            <> show totalSize
            <> "):\n\n"
            <> outputVars
            <> publicInputVars
            <> privateInputVars
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
          "    Ordinary constriants ("
            <> show ordinaryConstraintSize
            <> "):\n\n"
            <> unlines (map (\x -> "      " <> show x) cs)
            <> "\n"

    -- Boolean constraints
    showBooleanConstraints =
      if booleanConstraintSize == 0
        then ""
        else
          "    Boolean constriants ("
            <> show booleanConstraintSize
            <> "):\n\n"
            <> unlines (map ("      " <>) (prettyBooleanConstraints counters))
            <> "\n"

    -- BinRep constraints
    showBinRepConstraints =
      if totalBinRepConstraintSize == 0
        then ""
        else
          "    Binary representation constriants ("
            <> show totalBinRepConstraintSize
            <> "):\n\n"
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
