{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Keelung.Syntax.Counters
  ( Counters (Counters),
    SmallCounters (SmallCounters),
    VarKind (..),
    VarSort (..),
    getCount,
    setCount,
    reindex,
    prettyPrint,
  )
where

import Control.DeepSeq (NFData)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Serialize (Serialize)
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
    countIntermediate :: !SmallCounters -- counters for intermediate variables
  }
  deriving (Generic, NFData, Eq, Show)

instance Serialize Counters

instance Semigroup Counters where
  Counters cO1 cI1 cI2 <> Counters cO2 cI3 cI4 =
    Counters
      (cO1 <> cO2)
      (cI1 <> cI3)
      (cI2 <> cI4)

instance Monoid Counters where
  mempty = Counters mempty mempty mempty

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

--------------------------------------------------------------------------------

-- | Get the current count for a variable of the given kind and sort.
getCount :: VarSort -> VarKind -> Counters -> Int
getCount sort kind (Counters o i x) =
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

-- | Set the current count for a variable of the given kind and sort.
setCount :: VarSort -> VarKind -> Int -> Counters -> Counters
setCount sort kind n (Counters o i x) =
  case sort of
    OfOutput -> Counters (go o) i x
    OfInput -> Counters o (go i) x
    OfIntermediate -> Counters o i (go x)
  where
    go :: SmallCounters -> SmallCounters
    go (SmallCounters f b ubr u) =
      case kind of
        OfField -> SmallCounters n b ubr u
        OfBoolean -> SmallCounters f n ubr u
        OfUIntBinRep w -> SmallCounters f b (IntMap.insert w n ubr) u
        OfUInt w -> SmallCounters f b ubr (IntMap.insert w n u)

-- -- | Increment the counter for the given variable kind and sort.
-- bump :: VarSort -> VarKind -> Counters -> (Counters, Int)
-- bump = go
--   where
--     go :: VarSort -> VarKind -> Counters -> (Counters, Int)
--     go OfOutput OfField (Counters o i x) = let (o', n) = bumpF o in (Counters o' i x, n)
--     go OfOutput OfBoolean (Counters o i x) = let (o', n) = bumpB o in (Counters o' i x, n)
--     go OfOutput (OfUIntBinRep width) (Counters o i x) = let (o', n) = bumpUBR width o in (Counters o' i x, n)
--     go OfOutput (OfUInt width) (Counters o i x) = let (o', n) = bumpU width o in (Counters o' i x, n)
--     go OfInput OfField (Counters o i x) = let (i', n) = bumpF i in (Counters o i' x, n)
--     go OfInput OfBoolean (Counters o i x) = let (i', n) = bumpB i in (Counters o i' x, n)
--     go OfInput (OfUIntBinRep width) (Counters o i x) = let (x', n) = bumpUBR width x in (Counters o i x', n)
--     go OfInput (OfUInt width) (Counters o i x) = let (i', n) = bumpU width i in (Counters o i' x, n)
--     go OfIntermediate OfField (Counters o i x) = let (x', n) = bumpF x in (Counters o i x', n)
--     go OfIntermediate OfBoolean (Counters o i x) = let (x', n) = bumpB x in (Counters o i x', n)
--     go OfIntermediate (OfUIntBinRep width) (Counters o i x) = let (x', n) = bumpUBR width x in (Counters o i x', n)
--     go OfIntermediate (OfUInt width) (Counters o i x) = let (x', n) = bumpU width x in (Counters o i x', n)

--     -- Bump the counter for field element variables
--     bumpF :: SmallCounters -> (SmallCounters, Int)
--     bumpF (SmallCounters f b ubr u) = (SmallCounters (f + 1) b ubr u, f)

--     -- Bump the counter for Boolean variables
--     bumpB :: SmallCounters -> (SmallCounters, Int)
--     bumpB (SmallCounters f b ubr u) = (SmallCounters f (b + 1) ubr u, b)

--     -- Bump the counter for binary representations of unsigned integer variables
--     bumpUBR :: Width -> SmallCounters -> (SmallCounters, Int)
--     bumpUBR w (SmallCounters f b ubr u) =
--       (SmallCounters f b ubr' u, fromMaybe 0 (IntMap.lookup w ubr))
--       where
--         ubr' = IntMap.insertWith (+) w 1 ubr

--     -- Bump the counter for unsigned integer variables
--     bumpU :: Width -> SmallCounters -> (SmallCounters, Int)
--     bumpU w (SmallCounters f b ubr u) =
--       (SmallCounters f b ubr u', fromMaybe 0 (IntMap.lookup w u))
--       where
--         u' = IntMap.insertWith (+) w 1 u

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
