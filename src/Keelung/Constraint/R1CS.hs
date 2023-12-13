{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Constraint system for rank-1 constraints
module Keelung.Constraint.R1CS (R1CS (..), toR1Cs) where

import Control.DeepSeq (NFData)
import Data.IntMap.Strict qualified as IntMap
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Keelung.Constraint.R1C (R1C (..))
import Keelung.Data.FieldInfo (FieldInfo)
import Keelung.Data.Polynomial (Poly)
import Keelung.Data.Polynomial qualified as Poly
import Keelung.Syntax (Var, Width)
import Keelung.Syntax.Counters

--------------------------------------------------------------------------------

-- | Rank-1 Constraint System
data R1CS n = R1CS
  { -- | Info about the field
    r1csField :: FieldInfo,
    -- | List of constraints
    r1csConstraints :: [R1C n],
    -- | Variable bookkeeping
    r1csCounters :: Counters,
    -- | Hints for generating witnesses of EqZero constraints
    r1csEqZeros :: [(Poly n, Var)],
    -- | Hints for generating witnesses of DivMod constraints
    r1csDivMods :: [(Limbs, Limbs, Limbs, Limbs)],
    -- | Hints for generating witnesses of carry-less DivMod constraints
    r1csCLDivMods :: [(Limbs, Limbs, Limbs, Limbs)],
    -- | Hints for generating witnesses of ModInv constraints
    r1csModInvs :: [(Limbs, Limbs, Limbs, Integer)]
  }
  deriving (Generic, Eq, NFData, Functor)

instance (Serialize n) => Serialize (R1CS n)

instance (Num n, Eq n, Show n, Ord n) => Show (R1CS n) where
  show (R1CS _ cs counters _ _ _ _) =
    "R1CS {\n"
      <> prettyConstraints counters cs
      <> prettyVariables counters
      <> "}"

-- | Returns 'R1C's from a 'R1CS', including:
--   1. ordinary constraints
--   2. Boolean input variable constraints
--   3. binary representation constraints
toR1Cs :: (Num n, Eq n) => R1CS n -> [R1C n]
toR1Cs (R1CS _ ordinaryConstraints counters _ _ _ _) =
  ordinaryConstraints
    <> booleanInputVarConstraints
  where
    booleanInputVarConstraints =
      let generate (start, end) =
            map
              ( \var ->
                  R1C
                    (Right (Poly.singleVar var))
                    (Right (Poly.singleVar var))
                    (Right (Poly.singleVar var))
              )
              [start .. end - 1]
       in IntMap.toList (getBooleanConstraintRanges counters) >>= generate

--------------------------------------------------------------------------------

-- | A sequence of limbs & constants
type Limbs = [(Width, Either Var Integer)]