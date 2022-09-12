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
import Keelung.Field (N(..))
import qualified Data.List as List

--------------------------------------------------------------------------------

-- | Rank-1 Constraint System
data R1CS n = R1CS
  { -- List of constraints
    r1csConstraints :: [R1C n],
    -- Number of variables in the constraint system
    r1csNumOfVars :: Int,
    -- Number of input variables in the system
    -- Input variables are placed in the front
    -- (so we don't have to enumerate them all here)
    r1csNumOfInputVars :: Int,
    -- Set of Boolean input vars
    r1csBooleanInputVars :: IntSet,
    r1csOutputVars :: IntSet,
    r1csCNQZPairs :: [(Var, Var)]
  }
  deriving (Generic, Eq, NFData, Functor)

instance Serialize n => Serialize (R1CS n)

instance (Show n, Integral n, Bounded n, Fractional n) => Show (R1CS n) where
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
      <> show (IntSet.size os)
      <> "): "
      ++ outputVars
      ++ "\n}"
    where
      -- prettify input variables
      inputVars = case is of
        0 -> "none"
        1 -> "[ $0 ]"
        _ -> "[ $0 .. $" <> show (is - 1) <> " ]"
      -- prettify output variables
      outputVars =
        if IntSet.null os
          then "none"
          else "[ " <> List.intercalate ", " (map (\v -> "$" <> show v) (IntSet.toList os)) <> " ]"

      constraints = toR1Cs r1cs
      showConstraints =
        if null constraints
          then "none"
          else unlines (map (\s -> "    " ++ show (fmap N s)) constraints)

      showBooleanInputVarNumber =
        if null constraints
          then ""
          else "\n  Number of Boolean input variables: " <> show (IntSet.size bis)

-- | Return R1Cs from a R1CS
--   (includes constraints of boolean input variables)
toR1Cs :: (Num n, Eq n) => R1CS n -> [R1C n]
toR1Cs (R1CS cs _ _ bis _ _) = cs <> booleanInputVarConstraints
  where
    booleanInputVarConstraints :: (Num n, Eq n) => [R1C n]
    booleanInputVarConstraints =
      map
        ( \var ->
            R1C
              (Right (Poly.singleVar var))
              (Right (Poly.singleVar var))
              (Right (Poly.singleVar var))
        )
        (IntSet.toList bis)
