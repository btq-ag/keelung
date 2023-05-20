{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_HADDOCK hide #-}

{-# HLINT ignore "Replace case with fromMaybe" #-}

module Keelung.Syntax.Counters
  ( Counters (..),
    getTotalCount,
    -- | for constraint generation
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
    -- | for reading the counts and ranges of variables
    Category (..),
    ReadCounters,
    ReadType (..),
    getCount,
    Ranges,
    getRanges,
    getRange,
    enumerate,
    -- | for writing the counts and ranges of variables
    WriteType (..),
    addCount,
    -- | other helpers
    reindex,
    inRanges,
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
    countPublicInputSequence :: !(Seq WriteType), -- Sequence of public input variables
    countPrivateInputSequence :: !(Seq WriteType), -- Sequence of private input variables
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

setReducedCount :: Int -> Counters -> Counters
setReducedCount n (Counters o i1 i2 x s1 s2 _) = Counters o i1 i2 x s1 s2 n

-- | Total count of variables
getTotalCount :: Counters -> Int
getTotalCount (Counters o i1 i2 x _ _ reduced) =
  -- 'countReducedVarHack' should only have effect on intermediate variables
  (smallCounterSize o + smallCounterSize i1 + smallCounterSize i2) + (0 `max` (smallCounterSize x - reduced))

-- | For parsing raw inputs
getPublicInputSequence :: Counters -> Seq WriteType
getPublicInputSequence = countPublicInputSequence

getPrivateInputSequence :: Counters -> Seq WriteType
getPrivateInputSequence = countPrivateInputSequence

--------------------------------------------------------------------------------

-- | Re-index variables of different sorts and types
reindex :: Counters -> Category -> ReadType -> Var -> Var
reindex counters category typ index =
  getOffset counters (category, typ) + case typ of
    ReadBits width -> index * width
    _ -> index

--------------------------------------------------------------------------------

getBinReps :: Counters -> [BinRep]
getBinReps counters@(Counters o i1 i2 x _ _ _) =
  fromSmallCounter Output o ++ fromSmallCounter PublicInput i1 ++ fromSmallCounter PrivateInput i2 ++ fromSmallCounter Intermediate x
  where
    fromSmallCounter :: Category -> SmallCounters -> [BinRep]
    fromSmallCounter category (Struct _ _ u) = concatMap (fromPair category) (IntMap.toList u)

    fromPair :: Category -> (Width, Int) -> [BinRep]
    fromPair category (width, count) =
      let varOffset = reindex counters category (ReadUInt width) 0
          binRepOffset = reindex counters category (ReadBits width) 0
       in [BinRep (varOffset + index) width (binRepOffset + width * index) | index <- [0 .. count - 1]]

-- | Variables that needed to be constrained to be Boolean
--    1. Boolean output variables
--    2. UInt BinReps output variables
--    3. Boolean private input variables
--    4. UInt BinReps private input variables
--    5. Boolean public input variables
--    6. UInt BinReps public input variables
booleanConstraintCategories :: [(Category, ReadType)]
booleanConstraintCategories = [(Output, ReadBool), (Output, ReadAllBits), (PublicInput, ReadBool), (PublicInput, ReadAllBits), (PrivateInput, ReadBool), (PrivateInput, ReadAllBits)]

getBooleanConstraintCount :: Counters -> Int
getBooleanConstraintCount counters = sum $ map (getCount counters) booleanConstraintCategories

getBooleanConstraintRanges :: Counters -> [(Int, Int)]
getBooleanConstraintRanges counters = IntMap.toList $ getRanges counters booleanConstraintCategories

--------------------------------------------------------------------------------

prettyVariables :: Counters -> String
prettyVariables counters@(Counters o i1 i2 _ _ _ _) =
  let publicInputOffset = getOffset counters PublicInput
      privateInputOffset = getOffset counters PrivateInput
      outputOffset = getOffset counters Output
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

data Category = Output | PrivateInput | PublicInput | Intermediate
  deriving (Generic, NFData, Eq, Show)

data ReadType = ReadField | ReadBool | ReadAllBits | ReadAllUInt | ReadBits Width | ReadUInt Width
  deriving (Generic, NFData, Eq, Show)

instance Serialize ReadType

data WriteType = WriteField | WriteBool | WriteUInt Width
  deriving (Generic, NFData, Eq, Show)

instance Serialize WriteType

class ReadCounters selector where
  -- | get the total number of variables in that category
  getCount :: Counters -> selector -> Int

  getOffset :: Counters -> selector -> Int

-- | Access Counters of a given category.
instance ReadCounters Category where
  getCount counters category =
    getCount counters (category, ReadField)
      + getCount counters (category, ReadBool)
      + getCount counters (category, ReadAllBits)
      + getCount counters (category, ReadAllUInt)

  getOffset _ Output = 0
  getOffset counters PublicInput = getCount counters Output
  getOffset counters PrivateInput = getCount counters Output + getCount counters PublicInput
  getOffset counters Intermediate = getCount counters Output + getCount counters PublicInput + getCount counters PrivateInput

-- | Access Counters of a given type in a given category.
instance ReadCounters (Category, ReadType) where
  getCount counters (category, typ) =
    let selector = case category of
          Output -> countOutput
          PublicInput -> countPublicInput
          PrivateInput -> countPrivateInput
          Intermediate -> countIntermediate
     in case typ of
          ReadField -> structF (selector counters)
          ReadBool -> structB (selector counters)
          ReadAllBits -> binRepSize (structU (selector counters))
          ReadAllUInt -> uIntSize (structU (selector counters))
          ReadBits w -> case IntMap.lookup w (structU (selector counters)) of
            Nothing -> 0
            Just n -> n * w
          ReadUInt w -> case IntMap.lookup w (structU (selector counters)) of
            Nothing -> 0
            Just n -> n

  getOffset counters (category, typ) =
    let selector = case category of
          Output -> countOutput
          PublicInput -> countPublicInput
          PrivateInput -> countPrivateInput
          Intermediate -> countIntermediate
     in getOffset counters category + case typ of
          ReadField -> 0
          ReadBool -> structF (selector counters)
          ReadAllBits -> structF (selector counters) + structB (selector counters)
          ReadAllUInt -> structF (selector counters) + structB (selector counters) + binRepSize (structU (selector counters))
          ReadBits w ->
            structF (selector counters)
              + structB (selector counters)
              + sum
                ( IntMap.mapWithKey
                    ( \width count -> if w > width then count * width else 0
                    )
                    (structU (selector counters))
                )
          ReadUInt w ->
            structF (selector counters)
              + structB (selector counters)
              + binRepSize (structU (selector counters))
              + sum
                ( IntMap.filterWithKey
                    ( \width _ -> w > width
                    )
                    (structU (selector counters))
                )

addCount :: (Category, WriteType) -> Int -> Counters -> Counters
addCount (category, typ) n counters =
  -- (Counters o i1 i2 x s1 s2 r)
  case category of
    Output -> counters {countOutput = adjustSmallCounters (countOutput counters)}
    PublicInput -> counters {countPublicInput = adjustSmallCounters (countPublicInput counters), countPublicInputSequence = countPublicInputSequence counters <> newInputSequence}
    PrivateInput -> counters {countPrivateInput = adjustSmallCounters (countPrivateInput counters), countPrivateInputSequence = countPrivateInputSequence counters <> newInputSequence}
    Intermediate -> counters {countIntermediate = adjustSmallCounters (countIntermediate counters)}
  where
    adjustSmallCounters :: SmallCounters -> SmallCounters
    adjustSmallCounters (Struct f b u) = case typ of
      WriteField -> Struct (f + n) b u
      WriteBool -> Struct f (b + n) u
      WriteUInt w -> Struct f b (IntMap.insertWith (+) w n u)

    newInputSequence :: Seq WriteType
    newInputSequence = Seq.fromList $ replicate n typ

-- | Given a list of categories, get the ranges of variables in each category
getRanges :: ReadCounters selector => Counters -> [selector] -> Ranges
getRanges counters = buildRanges . map (\category -> (getOffset counters category, getCount counters category))

getRange :: ReadCounters selector => Counters -> selector -> Range
getRange counters selector = (getOffset counters selector, getCount counters selector)

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