{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_HADDOCK hide #-}

{-# HLINT ignore "Replace case with fromMaybe" #-}

module Keelung.Syntax.Counters
  ( Counters (..),
    VarType (..),
    VarSort (..),
    reindex,
    getCount,
    getTotalCount,
    addCount,
    -- | for constraint generation
    getOutputBinRepRange,
    getBinReps,
    getBooleanConstraintCount,
    getBooleanConstraintRanges,
    -- | for parsing raw inputs
    getPublicInputSequence,
    getPrivateInputSequence,
    -- | workaround for variable renumbering
    setReducedCount,
    -- | for pretty printing
    prettyConstraints,
    prettyVariables,
    prettyBooleanConstraints,
    -- | for querying the counts and ranges of variables
    getCounts,
    getRanges,
    enumerate,
    Ranges,
    inRanges,
    AccessCounters,
    Category (..),
    Type (..),
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
offsetOfType (Struct f b u) (OfUInt width) index =
  f
    + b
    + binRepSize u
    + sum (IntMap.filterWithKey (\width' _ -> width' < width) u)
    + index

--------------------------------------------------------------------------------

getOutputBinRepRange :: Counters -> (Int, Int)
getOutputBinRepRange counters =
  let start = offsetOfSort counters OfOutput + getCount OfOutput OfField counters + getCount OfOutput OfBoolean counters
      size = binRepSize (structU (countOutput counters))
   in (start, start + size)

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
--    3. Boolean private input variables
--    4. UInt BinReps private input variables
--    5. Boolean public input variables
--    6. UInt BinReps public input variables
booleanConstraintCategories :: [(Category, Type)]
booleanConstraintCategories = [(Output, Bool), (Output, AllBits), (PublicInput, Bool), (PublicInput, AllBits), (PrivateInput, Bool), (PrivateInput, AllBits)]

getBooleanConstraintCount :: Counters -> Int
getBooleanConstraintCount counters = sum $ getCounts counters booleanConstraintCategories

getBooleanConstraintRanges :: Counters -> [(Int, Int)]
getBooleanConstraintRanges counters = IntMap.toList $ getRanges counters booleanConstraintCategories

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

prettyConstraints :: Show constraint => Counters -> [constraint] -> [BinRep] -> String
prettyConstraints counters cs binReps =
  showConstraintSummary
    <> showOrdinaryConstraints
    <> showBooleanConstraints
    <> showBinRepConstraints
  where
    -- sizes of constraint groups
    totalBinRepConstraintSize = length binReps
    booleanConstraintSize = getBooleanConstraintCount counters
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
          "    Ordinary constraints ("
            <> show ordinaryConstraintSize
            <> "):\n\n"
            <> unlines (map (\x -> "      " <> show x) cs)
            <> "\n"

    -- Boolean constraints
    showBooleanConstraints =
      if booleanConstraintSize == 0
        then ""
        else
          "    Boolean constraints ("
            <> show booleanConstraintSize
            <> "):\n\n"
            <> unlines (map ("      " <>) (prettyBooleanConstraints counters))
            <> "\n"

    -- BinRep constraints
    showBinRepConstraints =
      if null binReps
        then ""
        else
          "    Binary representation constraints ("
            <> show (length binReps)
            <> "):\n\n"
            <> unlines (map (("      " <>) . show) binReps)
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

--------------------------------------------------------------------------------

type Range = (Int, Int)

type Ranges = IntMap Int

data Type = Field | Bool | AllBits | AllUInt | Bits Width | UInt Width

data Category = Output | PrivateInput | PublicInput | Intermediate

class AccessCounters selector where
  getCountTemp :: Counters -> selector -> Int
  getOffset :: Counters -> selector -> Int

-- | Access Counters of a given category.
instance AccessCounters Category where
  getCountTemp counters category =
    getCountTemp counters (category, Field)
      + getCountTemp counters (category, Bool)
      + getCountTemp counters (category, AllBits)
      + getCountTemp counters (category, AllUInt)

  getOffset _ Output = 0
  getOffset counters PublicInput = getCountTemp counters Output
  getOffset counters PrivateInput = getCountTemp counters Output + getCountTemp counters PublicInput
  getOffset counters Intermediate = getCountTemp counters Output + getCountTemp counters PublicInput + getCountTemp counters PrivateInput

-- | Access Counters of a given type in a given category.
instance AccessCounters (Category, Type) where
  getCountTemp counters (category, typ) =
    let selector = case category of
          Output -> countOutput
          PublicInput -> countPublicInput
          PrivateInput -> countPrivateInput
          Intermediate -> countIntermediate
     in case typ of
          Field -> structF (selector counters)
          Bool -> structB (selector counters)
          AllBits -> binRepSize (structU (selector counters))
          AllUInt -> uIntSize (structU (selector counters))
          Bits w -> case IntMap.lookup w (structU (selector counters)) of
            Nothing -> 0
            Just n -> n * w
          UInt w -> case IntMap.lookup w (structU (selector counters)) of
            Nothing -> 0
            Just n -> n

  getOffset counters (category, typ) =
    let selector = case category of
          Output -> countOutput
          PublicInput -> countPublicInput
          PrivateInput -> countPrivateInput
          Intermediate -> countIntermediate
     in getOffset counters category + case typ of
          Field -> 0
          Bool -> structF (selector counters)
          AllBits -> structF (selector counters) + structB (selector counters)
          AllUInt -> structF (selector counters) + structB (selector counters) + binRepSize (structU (selector counters))
          Bits w ->
            structF (selector counters)
              + structB (selector counters)
              + sum
                ( IntMap.mapWithKey
                    ( \width count -> if w > width then count * width else 0
                    )
                    (structU (selector counters))
                )
          UInt w ->
            structF (selector counters)
              + structB (selector counters)
              + binRepSize (structU (selector counters))
              + sum
                ( IntMap.filterWithKey
                    ( \width _ -> w > width
                    )
                    (structU (selector counters))
                )

-- | Given a list of categories,  get the total number of variables in each category
getCounts :: AccessCounters selector => Counters -> [selector] -> [Int]
getCounts = map . getCountTemp

-- | Given a list of categories, get the ranges of variables in each category
getRanges :: AccessCounters selector => Counters -> [selector] -> Ranges
getRanges counters categories = buildRanges $ map (\category -> (getOffset counters category, getCountTemp counters category)) categories

enumerate :: Ranges -> [Int]
enumerate ranges = concatMap (\(start, size) -> [start .. start + size - 1]) (IntMap.toList ranges)

-- | Merge overlapping segments into a list of non-overlapping segments
buildRanges :: [Range] -> Ranges
buildRanges = foldr build mempty
  where
    build :: Range -> Ranges -> Ranges
    build (_, 0) ranges = ranges
    build (start, size) ranges =
      case IntMap.lookupLE start ranges of -- find the segment that starts before the inserted segment
        Just (previous, previousSize) ->
          if start < previous + previousSize -- see if the inserted segment overlaps with the previous segment
            then IntMap.insert previous (max (start + size - previous) previousSize) ranges -- merge it with the previous segment
            else IntMap.insert start size ranges -- insert it as a new segment
        Nothing -> case IntMap.lookupGT start ranges of -- find the segment that starts after the inserted segment
          Just (next, nextSize) ->
            if next < start + size -- see if the inserted segment overlaps with the next segment
              then IntMap.insert start (max (next + nextSize - start) size) (IntMap.delete next ranges) -- merge it with the next segment
              else IntMap.insert start size ranges -- insert it as a new segment
          Nothing -> IntMap.insert start size ranges -- insert it as a new segment

inRanges :: Ranges -> Int -> Bool
inRanges ranges index = case IntMap.lookupLE index ranges of
  Nothing -> False
  Just (start, size) -> index < start + size