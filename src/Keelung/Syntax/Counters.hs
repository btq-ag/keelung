{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Keelung.Syntax.Counters where

import Control.DeepSeq (NFData)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import GHC.Generics (Generic)

------------------------------------------------------------------------------

type Var = Int

type Width = Int

-- | "Kinds" of variables.
data VarKind = OfField | OfBoolean | OfUIntBinRep Width | OfUInt Width

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

binRepSize :: IntMap Int -> Int
binRepSize = IntMap.foldlWithKey' (\acc width size -> acc + width * size) 0

smallCounterSize :: SmallCounters -> Int
smallCounterSize (SmallCounters f b ubr u) =
  f + b + binRepSize ubr + binRepSize u

--------------------------------------------------------------------------------

data Counters = Counters
  { countOutput :: !SmallCounters, -- counters for output variables
    countInput :: !SmallCounters, -- counters for input variables
    countIntermediate :: !SmallCounters -- counters for intermediate variables
  }
  deriving (Generic, NFData, Eq, Show)

choose :: VarSort -> Counters -> SmallCounters
choose OfOutput = countOutput
choose OfInput = countInput
choose OfIntermediate = countIntermediate

prettyPrint :: Counters -> [String]
prettyPrint counters@(Counters o i x) =
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

------------------------------------------------------------------------------

-- | Increment the counter for the given variable kind and sort.
bump :: VarSort -> VarKind -> Counters -> Counters
bump = go
  where
    go :: VarSort -> VarKind -> Counters -> Counters
    go OfOutput OfField (Counters o i x) = Counters (bumpF o) i x
    go OfOutput OfBoolean (Counters o i x) = Counters (bumpB o) i x
    go OfOutput (OfUIntBinRep width) (Counters o i x) = Counters (bumpUBR width o) i x
    go OfOutput (OfUInt width) (Counters o i x) = Counters (bumpU width o) i x
    go OfInput OfField (Counters o i x) = Counters o (bumpF i) x
    go OfInput OfBoolean (Counters o i x) = Counters o (bumpB i) x
    go OfInput (OfUIntBinRep width) (Counters o i x) = Counters o (bumpUBR width i) x
    go OfInput (OfUInt width) (Counters o i x) = Counters o (bumpU width i) x
    go OfIntermediate OfField (Counters o i x) = Counters o i (bumpF x)
    go OfIntermediate OfBoolean (Counters o i x) = Counters o i (bumpB x)
    go OfIntermediate (OfUIntBinRep width) (Counters o i x) = Counters o i (bumpUBR width x)
    go OfIntermediate (OfUInt width) (Counters o i x) = Counters o i (bumpU width x)

    -- Bump the counter for field element variables
    bumpF :: SmallCounters -> SmallCounters
    bumpF (SmallCounters f b ubr u) = SmallCounters (f + 1) b ubr u

    -- Bump the counter for Boolean variables
    bumpB :: SmallCounters -> SmallCounters
    bumpB (SmallCounters f b ubr u) = SmallCounters f (b + 1) ubr u

    -- Bump the counter for binary representations of unsigned integer variables
    bumpUBR :: Width -> SmallCounters -> SmallCounters
    bumpUBR w (SmallCounters f b ubr u) =
      SmallCounters f b ubr' u
      where
        ubr' = IntMap.insertWith (+) w 1 ubr

    -- Bump the counter for unsigned integer variables
    bumpU :: Width -> SmallCounters -> SmallCounters
    bumpU w (SmallCounters f b ubr u) =
      SmallCounters f b ubr u'
      where
        u' = IntMap.insertWith (+) w 1 u

--------------------------------------------------------------------------------

-- | Re-index variables of different sorts and kinds
reindex :: Counters -> VarSort -> VarKind -> Var -> Var
reindex counters sort kind index = offsetOfSort counters sort + offsetOfKind (choose sort counters) kind + index

offsetOfSort :: Counters -> VarSort -> Int
offsetOfSort _ OfOutput = 0
offsetOfSort counters OfInput = smallCounterSize (countOutput counters)
offsetOfSort counters OfIntermediate = offsetOfSort counters OfInput + smallCounterSize (countInput counters)

offsetOfKind :: SmallCounters -> VarKind -> Int
offsetOfKind _ OfField = 0
offsetOfKind (SmallCounters f _ _ _) OfBoolean = f
offsetOfKind (SmallCounters f b ubr _) (OfUIntBinRep width) = f + b + IntMap.size (IntMap.filterWithKey (\width' _ -> width' < width) ubr)
offsetOfKind (SmallCounters f b ubr u) (OfUInt width) = f + b + binRepSize ubr + IntMap.size (IntMap.filterWithKey (\width' _ -> width' < width) u)
