{-# LANGUAGE DeriveGeneric #-}

module Keelung.Constraint.R1CS where

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
  deriving (Generic, Eq)

instance Serialize n => Serialize (R1CS n)

instance (Show n, GaloisField n, Integral n, Bounded n) => Show (R1CS n) where
  show r1cs@(R1CS cs n is _ os _) =
    "R1CS {\n\
    \  R1C constraints ("
      <> show numberOfConstraints
      <> "):\n"
      <> showConstraints
      ++ "\n  number of variables: "
      ++ show n
      ++ "\n"
      ++ "  number of input vars: "
      ++ show is
      ++ "\n"
      ++ "  output vars: "
      ++ show (IntSet.toList os)
      ++ "\n"
      ++ "}"
    where
      numberOfConstraints = length cs + is
      showConstraints = unlines (map (\s -> "    " ++ show s) (toR1Cs r1cs))

-- | Return R1Cs from a R1CS
--   (includes boolean constraints for input variables)
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
