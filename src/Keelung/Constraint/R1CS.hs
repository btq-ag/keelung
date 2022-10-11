{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Keelung.Constraint.R1CS where

import Control.DeepSeq (NFData)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import qualified Keelung.Constraint.Polynomial as Poly
import Keelung.Constraint.R1C (R1C (..))
import Keelung.Types (Var)

--------------------------------------------------------------------------------

-- | Rank-1 Constraint System
--
--    Layout of variables:
--
--      ┌─────────────────┐
--      │     Input Vars  │
--      ├─────────────────┤
--      │    Output Vars  │
--      ├─────────────────┤
--      │                 │
--      │                 │
--      │  Ordinary Vars  │
--      │                 │
--      │                 │
--      └─────────────────┘
data R1CS n = R1CS
  { -- List of constraints
    r1csConstraints :: [R1C n],
    -- Number of all variables in the constraint system
    r1csVarSize :: Int,
    -- Number of input variables
    r1csInputVarSize :: Int,
    -- Set of Boolean variables
    r1csBoolVars :: IntSet,
    -- Number of output variables
    r1csOutputVarSize :: Int,
    -- For restoring CNQZ constraints during R1CS <-> ConstraintSystem conversion
    r1csCNEQs :: [CNEQ n]
  }
  deriving (Generic, Eq, NFData, Functor)

instance Serialize n => Serialize (R1CS n)

instance (Show n, Ord n, Eq n, Num n) => Show (R1CS n) where
  show r1cs@(R1CS cs n is bs os _) =
    "R1CS {\n\
    \  R1C constraints ("
      <> show (length cs + IntSet.size bs) -- as each Bool vars would introduce 1 extra constraints
      <> "): "
      <> showConstraints
      ++ "\n  Number of variables: "
      ++ show n
      <> showBooleanInputVarNumber
      ++ "\n  Input  variables ("
      <> show is
      <> "): "
      ++ inputVars
      ++ "\n  Output variables ("
      <> show os
      <> "): "
      ++ outputVars
      ++ "\n}"
    where
      -- prettify input variables
      inputVars = case is of
        0 -> "none"
        1 -> "$0"
        _ -> "$0 .. $" <> show (is - 1)
      -- prettify output variables
      outputVars = case os of
        0 -> "none"
        1 -> "$" <> show is
        _ -> "$" <> show is <> " .. $" <> show (is + os - 1)

      constraints = toR1Cs r1cs
      showConstraints =
        if null constraints
          then "none"
          else "\n\n" <> unlines (map (\s -> "    " <> show s) constraints)

      showBooleanInputVarNumber =
        if null constraints
          then ""
          else "\n  Number of Boolean variables: " <> show (IntSet.size bs)

-- | Return R1Cs from a R1CS
--   (includes constraints of boolean variables)
toR1Cs :: Num n => R1CS n -> [R1C n]
toR1Cs (R1CS cs _ _ bs _ _) = cs <> booleanInputVarConstraints
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