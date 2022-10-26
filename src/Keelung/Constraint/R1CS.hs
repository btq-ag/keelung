{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Keelung.Constraint.R1CS where

import Control.DeepSeq (NFData)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import qualified Keelung.Constraint.Polynomial as Poly
import Keelung.Constraint.R1C (R1C (..))
import Keelung.Syntax.VarCounters
import Keelung.Types

--------------------------------------------------------------------------------

-- | Rank-1 Constraint System
--
--    Layout of variables:
--
--      ┌─────────────────────────┐
--      │   Input Vars            │
--      ├─────────────────────────┤
--      │   Binary Representation |
--      │   of Number Input Vars  |
--      ├─────────────────────────┤
--      │   Output Vars           │
--      ├─────────────────────────┤
--      │                         │
--      │   Ordinary Vars         │
--      │                         │
--      └─────────────────────────┘
data R1CS n = R1CS
  { -- | List of constraints
    r1csConstraints :: [R1C n],
    -- | Variable bookkeeping
    r1csVarCounters :: VarCounters,
    -- | Set of Boolean variables
    r1csBoolVars :: IntSet,
    -- | For restoring CNQZ constraints during R1CS <-> ConstraintSystem conversion
    r1csCNEQs :: [CNEQ n],
    -- | For restoring binary representation of input variables
    r1csBinReps :: IntMap (Var, Int)
  }
  deriving (Generic, Eq, NFData, Functor)

instance Serialize n => Serialize (R1CS n)

instance (Num n, Eq n, Show n, Ord n) => Show (R1CS n) where
  show r1cs@(R1CS cs counters bs _ binReps) =
    "R1CS {\n"
      <> showTotalConstraintSize
      <> showOrdinaryConstraints
      <> showBooleanConstraints
      <> showBinRepConstraints
      <> indent (show counters)
      <> "}"
    where
      showTotalConstraintSize =
        "  Total constriant size: "
          <> show (length cs + IntSet.size bs + IntMap.size binReps)
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
        if IntSet.null bs
          then ""
          else
            "  Boolean constriants (" <> show (IntSet.size bs) <> "):\n"
              <> unlines
                ( map
                    (\var -> "    $" <> show var <> " = $" <> show var <> " * $" <> show var)
                    (IntSet.toList bs)
                )

      showBinRepConstraints =
        if IntSet.null bs
          then ""
          else
            "  Binary representation constriants (" <> show (IntMap.size binReps) <> "):\n"
              <> unlines
                ( map
                    (\(v, (b, n)) -> "    $" <> show v <> " = $" <> show b <> " + 2$" <> show (b + 1) <> " + ... + 2^" <> show (n - 1) <> "$" <> show (b + n - 1))
                    (IntMap.toList binReps)
                )

-- | Return R1Cs from a R1CS
--   (includes constraints of boolean variables)
toR1Cs :: (Num n, Eq n) => R1CS n -> [R1C n]
toR1Cs (R1CS cs _ bs _ binReps) = cs <> booleanInputVarConstraints <> binRepConstraints
  where
    booleanInputVarConstraints =
      map
        ( \var ->
            R1C
              (Right (Poly.singleVar var))
              (Right (Poly.singleVar var))
              (Right (Poly.singleVar var))
        )
        (IntSet.toList bs)

    binRepConstraints =
      map
        ( \(var, (b, n)) ->
            R1C
              (Left 1)
              (Poly.buildEither 0 [(var, 1)])
              (Poly.buildEither 0 [(b + i, fromInteger ((2 :: Integer) ^ i)) | i <- [0 .. n - 1]])
        )
        (IntMap.toList binReps)

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