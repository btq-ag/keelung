{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Constraint system for rank-1 constraints
module Keelung.Constraint.R1CS (R1CS (..), toR1Cs) where

import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Keelung.Constraint.R1C (R1C (..))
import Keelung.Data.BinRep (BinRep (..))
import Keelung.Data.Polynomial (Poly)
import Keelung.Data.Polynomial qualified as Poly
import Keelung.Field (FieldType)
import Keelung.Syntax (Var)
import Keelung.Syntax.Counters

--------------------------------------------------------------------------------

-- | Rank-1 Constraint System
data R1CS n = R1CS
  { -- | (Field type, characteristic, degree)
    r1csField :: (FieldType, Integer, Integer),
    -- | List of constraints
    r1csConstraints :: [R1C n],
    -- | List of binary representations
    r1csBinReps :: [BinRep],
    -- r1csBinReps :: ([BinRep], IntMap Int),
    -- | Variable bookkeeping
    r1csCounters :: Counters,
    -- | Hints for generating witnesses of EqZero constraints
    r1csEqZeros :: [(Poly n, Var)],
    -- | Hints for generating witnesses of DivMod constraints
    r1csDivMods :: [(Either (Var, Int) n, Either (Var, Int) n, Either (Var, Int) n, Either (Var, Int) n)],
    -- | Hints for generating witnesses of ModInv constraints
    r1csModInvs :: [(Either Var n, Either Var n, Integer)]
  }
  deriving (Generic, Eq, NFData, Functor)

instance Serialize n => Serialize (R1CS n)

instance (Num n, Eq n, Show n, Ord n) => Show (R1CS n) where
  show (R1CS _ cs binReps counters _ _ _) =
    "R1CS {\n"
      <> prettyConstraints counters cs binReps
      <> prettyVariables counters
      <> "}"

-- | Returns 'R1C's from a 'R1CS', including:
--   1. ordinary constraints
--   2. Boolean input variable constraints
--   3. binary representation constraints
toR1Cs :: (Num n, Eq n) => R1CS n -> [R1C n]
toR1Cs (R1CS _ ordinaryConstraints binReps counters _ _ _) =
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

-- --------------------------------------------------------------------------------
-- -- The encoding for constraint @x != y = out@ and some @m@ is:
-- --
-- --  > (x - y) * m = out
-- --  > (x - y) * (1 - out) = 0
-- data CNEQ n
--   = CNEQ
--       Var
--       -- ^ @x@: always a variable
--       (Either Var n)
--       -- ^ @y@: could be a variable or a constant
--       Var
--       -- ^ @m@: a constant
--   deriving
--     ( Generic,
--       Eq,
--       NFData,
--       Functor
--     )

-- instance Serialize n => Serialize (CNEQ n)

-- instance Show n => Show (CNEQ n) where
--   show (CNEQ x (Left y) m) = "Q $" <> show x <> " $" <> show y <> " $" <> show m
--   show (CNEQ x (Right y) m) = "Q $" <> show x <> " " <> show y <> " $" <> show m