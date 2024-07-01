{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Constraint system for rank-1 constraints
module Keelung.Constraint.R1CS (R1CS (..), toR1Cs) where

import Control.DeepSeq (NFData)
import Data.IntMap.Strict qualified as IntMap
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
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
    r1csConstraints :: Seq (R1C n),
    -- | Variable bookkeeping
    r1csCounters :: Counters,
    -- | Hints for generating witnesses of EqZero constraints
    r1csEqZeros :: Seq (Poly n, Var),
    -- | Hints for generating witnesses of DivMod constraints
    r1csDivMods :: Seq (Limbs, Limbs, Limbs, Limbs),
    -- | Hints for generating witnesses of carry-less DivMod constraints
    r1csCLDivMods :: Seq (Limbs, Limbs, Limbs, Limbs),
    -- | Hints for generating witnesses of ModInv constraints
    r1csModInvs :: Seq (Limbs, Limbs, Limbs, Integer)
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
toR1Cs :: (Num n, Eq n) => R1CS n -> Seq (R1C n)
toR1Cs (R1CS _ ordinaryConstraints counters _ _ _ _) =
  ordinaryConstraints
    <> booleanInputVarConstraints
  where
    booleanInputVarConstraints =
      let generate (start, size) =
            fmap
              ( \var ->
                  R1C
                    (Right (Poly.singleVar var))
                    (Right (Poly.singleVar var))
                    (Right (Poly.singleVar var))
              )
              (Seq.fromList [start .. start + size - 1])
       in Seq.fromList (IntMap.toList (getBooleanConstraintRanges counters)) >>= generate

--------------------------------------------------------------------------------

-- | A sequence of limbs & constants
type Limbs = [(Width, Either Var Integer)]