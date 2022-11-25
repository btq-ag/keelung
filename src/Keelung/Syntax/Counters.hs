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
  deriving (Generic, NFData, Eq)

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
  deriving (Generic, NFData, Eq)

choose :: VarSort -> Counters -> SmallCounters
choose OfOutput = countOutput
choose OfInput = countInput
choose OfIntermediate = countIntermediate

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
reindex counter sort kind index = offsetOfSort sort + offsetOfKind kind (choose sort counter) + index
  where
    offsetOfSort :: VarSort -> Int
    offsetOfSort OfOutput = 0
    offsetOfSort OfInput = smallCounterSize (countOutput counter)
    offsetOfSort OfIntermediate = offsetOfSort OfInput + smallCounterSize (countInput counter)

    offsetOfKind :: VarKind -> SmallCounters -> Int
    offsetOfKind OfField _ = 0
    offsetOfKind OfBoolean (SmallCounters f _ _ _) = f
    offsetOfKind (OfUIntBinRep width) (SmallCounters f b ubr _) = f + b + IntMap.size (IntMap.filterWithKey (\width' _ -> width' < width) ubr)
    offsetOfKind (OfUInt width) (SmallCounters f b ubr u) = f + b + binRepSize ubr + IntMap.size (IntMap.filterWithKey (\width' _ -> width' < width) u)
