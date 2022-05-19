{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Keelung.Field where

import Data.Euclidean (Euclidean, GcdDomain)
import Data.Field (Field)
import Data.Field.Galois (Binary, Prime)
import Data.Semiring (Ring, Semiring)

--------------------------------------------------------------------------------

-- | Common Galois fields
type GF64 = Binary 18446744073709551643

type GF181 = Prime 1552511030102430251236801561344621993261920897571225601

--------------------------------------------------------------------------------

-- | Data type for displaying field numbers nicely
-- Numbers in the second half of the field are represented as negative numbers
newtype N a = N {unN :: a}
  deriving (Eq, Ord)

deriving instance Bounded n => Bounded (N n)

deriving instance Field n => Field (N n)

deriving instance Euclidean n => Euclidean (N n)

deriving instance Ring n => Ring (N n)

deriving instance GcdDomain n => GcdDomain (N n)

deriving instance Semiring n => Semiring (N n)

deriving instance Fractional n => Fractional (N n)

deriving instance Num n => Num (N n)

instance (Show n, Bounded n, Integral n, Fractional n) => Show (N n) where
  show (N coeff) =
    let halfway = maxBound / 2
     in if coeff >= halfway
          then show (negate (toInteger (maxBound - coeff) + 1))
          else show (toInteger coeff)