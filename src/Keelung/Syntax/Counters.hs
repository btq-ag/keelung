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
    getBooleanConstraintCount,
    getBooleanConstraintRanges,
    -- | for parsing raw inputs
    getPublicInputSequence,
    getPrivateInputSequence,
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
    getUIntMap,
  )
where

import Control.DeepSeq (NFData)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Keelung.Data.Struct (Struct (..))

------------------------------------------------------------------------------

-- | Variables in Keelung are ordered as follows:
--    1. output variables
--    2. public input variables
--    3. private input variables
--    4. intermediate variables
--   Each of these categories has these different types of variables:
--    1. field variables
--    2. Boolean variables
--    3. unsigned integer bit variables

------------------------------------------------------------------------------

type Var = Int

type Width = Int

------------------------------------------------------------------------------

type SmallCounters = Struct Int Int Int

binRepSize :: IntMap Int -> Int
binRepSize = IntMap.foldlWithKey' (\acc width size -> acc + width * size) 0

smallCounterSize :: SmallCounters -> Int
smallCounterSize (Struct f b u) =
  f + b + binRepSize u

--------------------------------------------------------------------------------

data Counters = Counters
  { countOutput :: !SmallCounters, -- counters for output variables
    countPublicInput :: !SmallCounters, -- counters for public input variables
    countPrivateInput :: !SmallCounters, -- counters for private input variables
    countIntermediate :: !SmallCounters, -- counters for intermediate variables
    countPublicInputSequence :: !(Seq WriteType), -- Sequence of public input variables
    countPrivateInputSequence :: !(Seq WriteType) -- Sequence of private input variables
  }
  deriving (Generic, NFData, Eq, Show)

instance Serialize Counters

instance Semigroup Counters where
  Counters cOut1 cPubIn1 cPrivIn1 cInt1 cPubInSeq1 cPrivInSeq1 <> Counters cOut2 cPubIn2 cPrivIn2 cInt2 cPubInSeq2 cPrivInSeq2 =
    Counters
      (addSmallCounters cOut1 cOut2)
      (addSmallCounters cPubIn1 cPubIn2)
      (addSmallCounters cPrivIn1 cPrivIn2)
      (addSmallCounters cInt1 cInt2)
      (cPubInSeq1 <> cPubInSeq2)
      (cPrivInSeq1 <> cPrivInSeq2)
    where
      addSmallCounters :: SmallCounters -> SmallCounters -> SmallCounters
      addSmallCounters (Struct f1 b1 u1) (Struct f2 b2 u2) =
        Struct (f1 + f2) (b1 + b2) (IntMap.unionWith (+) u1 u2)

instance Monoid Counters where
  mempty = Counters (Struct 0 0 mempty) (Struct 0 0 mempty) (Struct 0 0 mempty) (Struct 0 0 mempty) mempty mempty

-- | Total count of variables
getTotalCount :: Counters -> Int
getTotalCount (Counters o i1 i2 x _ _) =
  smallCounterSize o + smallCounterSize i1 + smallCounterSize i2 + smallCounterSize x

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
    ReadUInt width -> index * width
    _ -> index

--------------------------------------------------------------------------------

-- | Variables that needed to be constrained to be Boolean
--    1. Boolean output variables
--    2. UInt output bit variables
--    3. Boolean public input variables
--    4. UInt public input bit variables
--    5. Boolean private input variables
--    6. UInt private input bit variables
booleanConstraintCategories :: [(Category, ReadType)]
booleanConstraintCategories = [(Output, ReadBool), (Output, ReadAllUInts), (PublicInput, ReadBool), (PublicInput, ReadAllUInts), (PrivateInput, ReadBool), (PrivateInput, ReadAllUInts)]

getBooleanConstraintCount :: Counters -> Int
getBooleanConstraintCount counters = sum $ map (getCount counters) booleanConstraintCategories

getBooleanConstraintRanges :: Counters -> [(Int, Int)]
getBooleanConstraintRanges counters = IntMap.toList $ getRanges counters booleanConstraintCategories

--------------------------------------------------------------------------------

prettyVariables :: Counters -> String
prettyVariables counters =
  let totalSize = getTotalCount counters
      (outputStart, outputCount) = getRange counters Output
      outputVars = case outputCount of
        0 -> ""
        1 -> "    Output variable : $" <> show outputStart <> "\n"
        _ -> "    Output variables: $" <> show outputStart <> " ... $" <> show (outputStart + outputCount - 1) <> "\n"

      (publicInputStart, publicInputCount) = getRange counters PublicInput
      publicInputVars = case publicInputCount of
        0 -> ""
        1 -> "    Public Input variable : $" <> show publicInputStart <> "\n"
        _ -> "    Public Input variables: $" <> show publicInputStart <> " ... $" <> show (publicInputCount + publicInputCount - 1) <> "\n"

      (privateInputStart, privateInputCount) = getRange counters PrivateInput
      privateInputVars = case privateInputCount of
        0 -> ""
        1 -> "    Private Input variable : $" <> show privateInputStart <> "\n"
        _ -> "    Private Input variables: $" <> show privateInputStart <> " ... $" <> show (privateInputCount + privateInputCount - 1) <> "\n"

      (otherStart, otherCount) = getRange counters Intermediate
      otherVars = case otherCount of
        0 -> ""
        1 -> "    Other variable : $" <> show otherStart <> "\n"
        _ -> "    Other variables: $" <> show otherStart <> " ... $" <> show (otherStart + otherCount - 1) <> "\n"
   in if totalSize == 0
        then ""
        else
          "  Variables ("
            <> show totalSize
            <> "):\n\n"
            <> outputVars
            <> publicInputVars
            <> privateInputVars
            <> otherVars
            <> "\n"

prettyConstraints :: Show constraint => Counters -> [constraint] -> String
prettyConstraints counters cs =
  showConstraintSummary
    <> showOrdinaryConstraints
    <> showBooleanConstraints
  where
    -- sizes of constraint groups
    booleanConstraintSize = getBooleanConstraintCount counters
    ordinaryConstraintSize = length cs

    -- summary of constraint groups
    showConstraintSummary =
      "  Constriant ("
        <> show (ordinaryConstraintSize + booleanConstraintSize)
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

prettyBooleanConstraints :: Counters -> [String]
prettyBooleanConstraints counters =
  concatMap showSegment (getBooleanConstraintRanges counters)
  where
    showSegment :: (Int, Int) -> [String]
    showSegment (start, count) =
      case count of
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
            showBooleanConstraint (start + count - 1)
          ]

    showBooleanConstraint :: Int -> String
    showBooleanConstraint n = "$" <> show n <> " = $" <> show n <> " * $" <> show n

--------------------------------------------------------------------------------

type Range = (Int, Int)

type Ranges = IntMap Int

data Category = Output | PrivateInput | PublicInput | Intermediate
  deriving (Generic, NFData, Eq, Show)

data ReadType = ReadField | ReadBool | ReadAllUInts | ReadUInt Width
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
      + getCount counters (category, ReadAllUInts)

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
          ReadAllUInts -> binRepSize (structU (selector counters))
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
          ReadAllUInts -> structF (selector counters) + structB (selector counters)
          ReadUInt w ->
            structF (selector counters)
              + structB (selector counters)
              + sum
                ( IntMap.mapWithKey
                    ( \width count -> if w > width then count * width else 0
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
          if start <= previous + previousSize -- see if the inserted segment overlaps with the previous segment
            then IntMap.insert previous (max (start + size - previous) previousSize) ranges -- merge it with the previous segment
            else IntMap.insert start size ranges -- insert it as a new segment
        Nothing -> case IntMap.lookupGE start ranges of -- find the segment that starts after the inserted segment
          Just (next, nextSize) ->
            if next <= start + size -- see if the inserted segment overlaps with the next segment
              then IntMap.insert start (max (next + nextSize - start) size) (IntMap.delete next ranges) -- merge it with the next segment
              else IntMap.insert start size ranges -- insert it as a new segment
          Nothing -> IntMap.insert start size ranges -- insert it as a new segment

inRanges :: Ranges -> Int -> Bool
inRanges ranges index = case IntMap.lookupLE index ranges of
  Nothing -> False
  Just (start, size) -> index < start + size

getUIntMap :: Counters -> Category -> IntMap Int
getUIntMap counters Output = structU (countOutput counters)
getUIntMap counters PublicInput = structU (countPublicInput counters)
getUIntMap counters PrivateInput = structU (countPrivateInput counters)
getUIntMap counters Intermediate = structU (countIntermediate counters)