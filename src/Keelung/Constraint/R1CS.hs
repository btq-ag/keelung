{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Keelung.Constraint.R1CS where

import Control.DeepSeq (NFData)
import Data.Field.Galois (GaloisField)
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
--
data R1CS n = R1CS
  { -- List of constraints
    r1csConstraints :: [R1C n],
    -- Number of all variables in the constraint system
    r1csVarSize :: Int,
    -- Number of input variables
    r1csInputVarSize :: Int,
    -- Set of Boolean input variables
    r1csBoolInputVars :: IntSet,
    -- Number of output variables
    r1csOutputVarSize :: Int,
    -- List of pairs for restoring CNQZ constraints during R1CS <-> ConstraintSystem conversion
    r1csCNQZPairs :: [(Var, Var)]
  }
  deriving (Generic, Eq, NFData, Functor)

instance Serialize n => Serialize (R1CS n)

instance (GaloisField n, Integral n) => Show (R1CS n) where
  show r1cs@(R1CS cs n is bis os _) =
    "R1CS {\n\
    \  R1C constraints ("
      <> show (length cs + IntSet.size bis)
      <> "):\n\n"
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
          else unlines (map (\s -> "    " ++ show s) constraints)

      showBooleanInputVarNumber =
        if null constraints
          then ""
          else "\n  Number of Boolean input variables: " <> show (IntSet.size bis)

-- | Return R1Cs from a R1CS
--   (includes constraints of boolean input variables)
toR1Cs :: GaloisField n => R1CS n -> [R1C n]
toR1Cs (R1CS cs _ _ bis _ _) = cs <> booleanInputVarConstraints
  where
    booleanInputVarConstraints :: GaloisField n => [R1C n]
    booleanInputVarConstraints =
      map
        ( \var ->
            R1C
              (Right (Poly.singleVar var))
              (Right (Poly.singleVar var))
              (Right (Poly.singleVar var))
        )
        (IntSet.toList bis)
