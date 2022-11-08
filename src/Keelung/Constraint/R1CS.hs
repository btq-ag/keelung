{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Keelung.Constraint.R1CS where

import Control.DeepSeq (NFData)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import qualified Keelung.Constraint.Polynomial as Poly
import Keelung.Constraint.R1C (R1C (..))
import Keelung.Syntax.VarCounters
import Keelung.Types

--------------------------------------------------------------------------------

-- | Rank-1 Constraint System
data R1CS n = R1CS
  { -- | List of constraints
    r1csConstraints :: [R1C n],
    -- | Variable bookkeeping
    r1csVarCounters :: VarCounters,
    -- | For restoring CNQZ constraints during R1CS <-> ConstraintSystem conversion
    r1csCNEQs :: [CNEQ n],
    -- | For restoring binary representation of Number input variables
    --   [(inputIndex, binRepIndex)]
    r1csNumBinReps :: IntMap Var,
    -- | For restoring binary representation of custom output variables
    --   [(bitWidth, [(inputIndex, binRepIndex)])]
    r1csCustomBinReps :: IntMap (IntMap Var)
  }
  deriving (Generic, Eq, NFData, Functor)

instance Serialize n => Serialize (R1CS n)

instance (Num n, Eq n, Show n, Ord n) => Show (R1CS n) where
  show r1cs@(R1CS cs counters _ numBinReps customBinReps) =
    "R1CS {\n"
      <> showTotalConstraintSize
      <> showOrdinaryConstraints
      <> showBooleanConstraints
      <> showBinRepConstraints
      <> indent (show counters)
      <> "}"
    where
      numBitWidth = getNumBitWidth counters

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

      showBinRepConstraints =
        if totalBinRepConstraintSize == 0
          then ""
          else
            "  Binary representation constriants (" <> show (numInputVarSize counters + totalCustomInputSize counters) <> "):\n"
              <> unlines
                ( map
                    (uncurry (showBinRepConstraint numBitWidth))
                    (IntMap.toList numBinReps)
                    ++ concatMap
                      ( \(bitWidth, pairs) ->
                          map
                            (uncurry (showBinRepConstraint bitWidth))
                            (IntMap.toList pairs)
                      )
                      (IntMap.toList customBinReps)
                )
        where
          showBinRepConstraint 2 var binRep =
            "    $"
              <> show var
              <> " = $"
              <> show binRep
              <> " + 2$"
              <> show (binRep + 1)
          showBinRepConstraint 3 var binRep =
            "    $"
              <> show var
              <> " = $"
              <> show binRep
              <> " + 2$"
              <> show (binRep + 1)
              <> " + 4$"
              <> show (binRep + 2)
          showBinRepConstraint width var binRep =
            "    $"
              <> show var
              <> " = $"
              <> show binRep
              <> " + 2$"
              <> show (binRep + 1)
              <> " + ... + 2^"
              <> show (width - 1)
              <> "$"
              <> show (binRep + width - 1)

-- | Return R1Cs from a R1CS
--   (includes constraints of boolean variables)
toR1Cs :: (Num n, Eq n) => R1CS n -> [R1C n]
toR1Cs (R1CS cs counters _ numBinReps customBinReps) =
  cs
    <> booleanInputVarConstraints
    <> numBinRepConstraints
    <> customBinRepConstraints
  where
    numBitWidth = getNumBitWidth counters

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
        ( \(input, binRep) ->
            R1C
              (Left 1)
              (Poly.buildEither 0 [(input, 1)])
              (Poly.buildEither 0 [(binRep + i, fromInteger ((2 :: Integer) ^ i)) | i <- [0 .. numBitWidth - 1]])
        )
        (IntMap.toList numBinReps)

    customBinRepConstraints =
      concatMap
        ( \(width, pairs) ->
            map
              ( \(input, binRep) ->
                  R1C
                    (Left 1)
                    (Poly.buildEither 0 [(input, 1)])
                    (Poly.buildEither 0 [(binRep + i, fromInteger ((2 :: Integer) ^ i)) | i <- [0 .. width - 1]])
              )
              (IntMap.toList pairs)
        )
        (IntMap.toList customBinReps)

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