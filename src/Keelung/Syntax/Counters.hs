{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Keelung.Syntax.Counters
  ( Counters (Counters),
    SmallCounters (SmallCounters),
    VarKind (..),
    VarSort (..),
    getCount,
    getInputSequence,
    getInputVarRange,
    addCount,
    reindex,
    prettyPrint,
  )
where

import Control.DeepSeq (NFData)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

------------------------------------------------------------------------------

type Var = Int

type Width = Int

-- | "Kinds" of variables.
data VarKind = OfField | OfBoolean | OfUIntBinRep Width | OfUInt Width
  deriving (Generic, NFData, Eq, Show)

instance Serialize VarKind

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

smallCounterSize :: SmallCounters -> Int
smallCounterSize (SmallCounters f b ubr u) =
  f + b + binRepSize ubr + binRepSize u

--------------------------------------------------------------------------------

data Counters = Counters
  { countOutput :: !SmallCounters, -- counters for output variables
    countInput :: !SmallCounters, -- counters for input variables
    countIntermediate :: !SmallCounters, -- counters for intermediate variables
    countInputSequence :: !(Seq (VarKind, Int)) -- Sequence of input variables
  }
  deriving (Generic, NFData, Eq, Show)

instance Serialize Counters

instance Semigroup Counters where
  Counters cOut1 cIn1 cInt1 cInSeq1 <> Counters cOut2 cIn2 cInt2 cInSeq2 =
    Counters
      (cOut1 <> cOut2)
      (cIn1 <> cIn2)
      (cInt1 <> cInt2)
      (cInSeq1 <> cInSeq2)

instance Monoid Counters where
  mempty = Counters mempty mempty mempty mempty

choose :: VarSort -> Counters -> SmallCounters
choose OfOutput = countOutput
choose OfInput = countInput
choose OfIntermediate = countIntermediate

prettyPrint :: Counters -> [String]
prettyPrint counters@(Counters o i x _) =
  let inputOffset = offsetOfSort counters OfInput
      outputOffset = offsetOfSort counters OfOutput
   in ["Total variable size: " <> show (smallCounterSize o + smallCounterSize i + smallCounterSize x)]
        <> case smallCounterSize o of
          0 -> []
          1 -> ["Output variable : $" <> show inputOffset]
          n -> ["Output variables: $" <> show inputOffset <> " .. $" <> show (inputOffset + n - 1)]
        <> case smallCounterSize i of
          0 -> []
          1 -> ["Input variable  : $" <> show outputOffset]
          n -> ["Input variables : $" <> show outputOffset <> " .. $" <> show (outputOffset + n - 1)]

--------------------------------------------------------------------------------

-- | Get the current count for a variable of the given kind and sort.
getCount :: VarSort -> VarKind -> Counters -> Int
getCount sort kind (Counters o i x _) =
  case sort of
    OfOutput -> go o
    OfInput -> go i
    OfIntermediate -> go x
  where
    go :: SmallCounters -> Int
    go (SmallCounters f b ubr u) =
      case kind of
        OfField -> f
        OfBoolean -> b
        OfUIntBinRep w -> IntMap.findWithDefault 0 w ubr
        OfUInt w -> IntMap.findWithDefault 0 w u

getInputSequence :: Counters -> Seq (VarKind, Int)
getInputSequence = countInputSequence

getInputVarRange :: Counters -> (Int, Int)
getInputVarRange counters =
  let inputOffset = offsetOfSort counters OfInput
      inputSize = smallCounterSize (countInput counters)
   in (inputOffset, inputOffset + inputSize - 1)

-- | Set the current count for a variable of the given kind and sort.
addCount :: VarSort -> VarKind -> Int -> Counters -> Counters
addCount sort kind n (Counters o i x is) =
  case sort of
    OfOutput -> Counters (adjustSmallCounters o) i x (is <> newInputSequence)
    OfInput -> Counters o (adjustSmallCounters i) x (is <> newInputSequence)
    OfIntermediate -> Counters o i (adjustSmallCounters x) (is <> newInputSequence)
  where
    adjustSmallCounters :: SmallCounters -> SmallCounters
    adjustSmallCounters (SmallCounters f b ubr u) =
      case kind of
        OfField -> SmallCounters (f + n) b ubr u
        OfBoolean -> SmallCounters f (f + n) ubr u
        OfUIntBinRep _ -> error "[ panic ] Should use `OfUInt` to adjust the counter instead"
        OfUInt w -> SmallCounters f b (IntMap.insertWith (+) w n ubr) (IntMap.insertWith (+) w n u)

    oldCount = getCount sort kind (Counters o i x is)

    newInputSequence :: Seq (VarKind, Int)
    newInputSequence = Seq.fromList [(kind, index) | index <- [oldCount .. oldCount + n - 1]]

--------------------------------------------------------------------------------

-- | Re-index variables of different sorts and kinds
reindex :: Counters -> VarSort -> VarKind -> Var -> Var
reindex counters sort kind index = offsetOfSort counters sort + offsetOfKind (choose sort counters) kind + index

offsetOfSort :: Counters -> VarSort -> Int
offsetOfSort _ OfOutput = 0
offsetOfSort counters OfInput = smallCounterSize (countOutput counters)
offsetOfSort counters OfIntermediate = smallCounterSize (countOutput counters) + smallCounterSize (countInput counters)

offsetOfKind :: SmallCounters -> VarKind -> Int
offsetOfKind _ OfField = 0
offsetOfKind (SmallCounters f _ _ _) OfBoolean = f
offsetOfKind (SmallCounters f b ubr _) (OfUIntBinRep width) = f + b + IntMap.size (IntMap.filterWithKey (\width' _ -> width' < width) ubr)
offsetOfKind (SmallCounters f b ubr u) (OfUInt width) = f + b + binRepSize ubr + IntMap.size (IntMap.filterWithKey (\width' _ -> width' < width) u)
