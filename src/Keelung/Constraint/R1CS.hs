{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Keelung.Constraint.R1CS where

import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import qualified Keelung.Constraint.Polynomial as Poly
import Keelung.Constraint.R1C (R1C (..))
import Keelung.Syntax.BinRep (BinRep (..), BinReps)
import qualified Keelung.Syntax.BinRep as BinRep
import Keelung.Syntax.VarCounters
import Keelung.Types
import Keelung.Syntax.Counters

--------------------------------------------------------------------------------

-- | Rank-1 Constraint System
data R1CS n = R1CS
  { -- | List of constraints
    r1csConstraints :: [R1C n],
    -- | Variable bookkeeping
    r1csVarCounters :: VarCounters,
    -- | Variable bookkeeping
    r1csCounters :: Counters,
    -- | For restoring CNQZ constraints during R1CS <-> ConstraintSystem conversion
    r1csCNEQs :: [CNEQ n],
    -- | For restoring binary representation of Number input variables
    r1csNumBinReps :: BinReps,
    -- | For restoring binary representation of custom output variables
    --   [(bitWidth, [(inputIndex, binRepIndex)])]
    r1csCustomBinReps :: BinReps
  }
  deriving (Generic, Eq, NFData, Functor)

instance Serialize n => Serialize (R1CS n)

instance (Num n, Eq n, Show n, Ord n) => Show (R1CS n) where
  show r1cs@(R1CS cs counters _ _ numBinReps customBinReps) =
    "R1CS {\n"
      <> showTotalConstraintSize
      <> showOrdinaryConstraints
      <> showBooleanConstraints
      <> show (numBinReps <> customBinReps)
      <> indent (show counters)
      <> "}"
    where
      totalBinRepConstraintSize = numInputVarSize counters + totalCustomInputSize counters
      showTotalConstraintSize =
        "  Total constriant size: "
          <> show (length cs + snd (boolVarsRange counters) - fst (boolVarsRange counters) + totalBinRepConstraintSize)
          <> "\n"
      -- constraints excluding Boolean constraints & Binary Representation constraints
      ordinaryConstraints = r1csConstraints r1cs
      showOrdinaryConstraints =
        if null ordinaryConstraints
          then "  Ordinary constraints: None"
          else
            "  Ordinary constraints: \n"
              <> unlines (map (\s -> "    " <> show s) ordinaryConstraints)

      showBooleanConstraints =
        let (start, end) = boolVarsRange counters
            showBooleanConstraint n =
              "    $"
                <> show n
                <> " = $"
                <> show n
                <> " * $"
                <> show n
                <> "\n"
         in case end - start of
              0 -> ""
              1 ->
                "  Boolean constriants (1):\n"
                  <> showBooleanConstraint start
              2 ->
                "  Boolean constriants (2):\n"
                  <> showBooleanConstraint start
                  <> showBooleanConstraint (start + 1)
              3 ->
                "  Boolean constriants (3):\n"
                  <> showBooleanConstraint start
                  <> showBooleanConstraint (start + 1)
                  <> showBooleanConstraint (start + 2)
              n ->
                "  Boolean constriants (" <> show n <> "):\n"
                  <> showBooleanConstraint start
                  <> "      ..\n"
                  <> showBooleanConstraint (end - 1)

-- | Return R1Cs from a R1CS
--   (includes constraints of boolean variables)
toR1Cs :: (Num n, Eq n) => R1CS n -> [R1C n]
toR1Cs (R1CS cs counters _ _ numBinReps customBinReps) =
  cs
    <> booleanInputVarConstraints
    <> numBinRepConstraints
    <> customBinRepConstraints
  where
    booleanInputVarConstraints =
      let (start, end) = boolVarsRange counters
       in map
            ( \var ->
                R1C
                  (Right (Poly.singleVar var))
                  (Right (Poly.singleVar var))
                  (Right (Poly.singleVar var))
            )
            [start .. end - 1]

    numBinRepConstraints =
      map
        ( \binRep ->
            R1C
              (Left 1)
              (Poly.buildEither 0 [(binRepVar binRep, 1)])
              (Poly.buildEither 0 [(binRepBitsIndex binRep + i, fromInteger ((2 :: Integer) ^ i)) | i <- [0 .. binRepBitWidth binRep - 1]])
        )
        (BinRep.toList numBinReps)

    customBinRepConstraints =
      map
        ( \binRep ->
            R1C
              (Left 1)
              (Poly.buildEither 0 [(binRepVar binRep, 1)])
              (Poly.buildEither 0 [(binRepBitsIndex binRep + i, fromInteger ((2 :: Integer) ^ i)) | i <- [0 .. binRepBitWidth binRep - 1]])
        )
        (BinRep.toList customBinReps)

--------------------------------------------------------------------------------

-- For restoring CNQZ constraints during R1CS <-> ConstraintSystem conversion

-- Constraint 'x != y = out'
-- The encoding is, for some 'm':
--  1. (x - y) * m = out
--  2. (x - y) * (1 - out) = 0
data CNEQ n
  = CNEQ
      (Either Var n) -- 'x' could be a variable or a constant
      (Either Var n) -- 'y' could be a variable or a constant
      Var -- m
  deriving (Generic, Eq, NFData, Functor)

instance Serialize n => Serialize (CNEQ n)

instance Show n => Show (CNEQ n) where
  show (CNEQ (Left x) (Left y) m) = "Q $" <> show x <> " $" <> show y <> " $" <> show m
  show (CNEQ (Left x) (Right y) m) = "Q $" <> show x <> " " <> show y <> " $" <> show m
  show (CNEQ (Right x) (Left y) m) = "Q " <> show x <> " $" <> show y <> " $" <> show m
  show (CNEQ (Right x) (Right y) m) = "Q " <> show x <> " " <> show y <> " $" <> show m