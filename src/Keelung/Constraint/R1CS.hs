{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Constraint system for rank-1 constraints
module Keelung.Constraint.R1CS (R1CS (..), toR1Cs, CNEQ (..)) where

import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Keelung.Constraint.R1C (R1C (..))
import Keelung.Data.BinRep (BinRep (..))
import Keelung.Data.Polynomial qualified as Poly
import Keelung.Syntax (Var)
import Keelung.Syntax.Counters

--------------------------------------------------------------------------------

-- | Rank-1 Constraint System
data R1CS n = R1CS
  { -- | List of constraints
    r1csConstraints :: [R1C n],
    -- | List of binary representations
    r1csBinReps :: [BinRep],
    -- | Variable bookkeeping
    r1csCounters :: Counters,
    -- | Hints for generating witnesses of CNQZ constraints
    r1csCNEQs :: [CNEQ n],
    -- | Hints for generating witnesses of DivMod constraints
    r1csDivMods :: [(Either Var n, Either Var n, Either Var n, Either Var n)],
    -- | Hints for generating witnesses of ModInv constraints
    r1csModInvs :: [(Either Var n, Either Var n, Integer)]
  }
  deriving (Generic, Eq, NFData, Functor)

instance Serialize n => Serialize (R1CS n)

instance (Num n, Eq n, Show n, Ord n) => Show (R1CS n) where
  show (R1CS cs binReps counters _ _ _) =
    "R1CS {\n"
      <> prettyConstraints counters cs binReps
      <> prettyVariables counters
      <> "}"

-- | Returns 'R1C's from a 'R1CS', including:
--   1. ordinary constraints
--   2. Boolean input variable constraints
--   3. binary representation constraints
toR1Cs :: (Num n, Eq n) => R1CS n -> [R1C n]
toR1Cs (R1CS ordinaryConstraints binReps counters _ _ _) =
  ordinaryConstraints
    <> booleanInputVarConstraints
    <> binRepConstraints
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
       in concatMap generate (getBooleanConstraintRanges counters)

    binRepConstraints =
      map
        ( \(BinRep fVar width bVar) ->
            R1C
              (Poly.buildEither 0 [(bVar + i, 2 ^ i) | i <- [0 .. width - 1]])
              (Left 1)
              (Right (Poly.singleVar fVar))
        )
        binReps

-- binRepConstraints =
--   map
--     ( \(BinRep fVar width bVar) ->
--         R1C
--           (Poly.buildEither 0 [(bVar + i, 2 ^ i) | i <- [0 .. width - 1]])
--           (Left 1)
--           (Right (Poly.singleVar fVar))
--     )
--     (getBinReps counters)

--------------------------------------------------------------------------------

-- | For restoring CNQZ constraints during R1CS \<=\> ConstraintSystem conversion
--
-- The encoding for constraint @x != y = out@ and some @m@ is:
--
--  > (x - y) * m = out
--  > (x - y) * (1 - out) = 0
data CNEQ n
  = CNEQ
      (Either Var n)
      -- ^ @x@: could be a variable or a constant
      (Either Var n)
      -- ^ @y@: could be a variable or a constant
      Var
      -- ^ @m@: a constant
  deriving
    ( Generic,
      Eq,
      NFData,
      Functor
    )

instance Serialize n => Serialize (CNEQ n)

instance Show n => Show (CNEQ n) where
  show (CNEQ (Left x) (Left y) m) = "Q $" <> show x <> " $" <> show y <> " $" <> show m
  show (CNEQ (Left x) (Right y) m) = "Q $" <> show x <> " " <> show y <> " $" <> show m
  show (CNEQ (Right x) (Left y) m) = "Q " <> show x <> " $" <> show y <> " $" <> show m
  show (CNEQ (Right x) (Right y) m) = "Q " <> show x <> " " <> show y <> " $" <> show m