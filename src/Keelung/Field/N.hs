{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Keelung.Field.N where

import Data.Euclidean (Euclidean, GcdDomain)
import Data.Field (Field)
import Data.Semiring (Ring, Semiring)

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

deriving instance Enum n => Enum (N n)

deriving instance Real n => Real (N n)

instance (Integral n, Bounded n, Fractional n) => Integral (N n) where
  quotRem n m = (N q, N r)
    where
      (q, r) = quotRem (unN n) (unN m)
  toInteger (N x) =
    let halfway = maxBound / 2
     in if x >= halfway
          then negate (toInteger (maxBound - x) + 1)
          else toInteger x

instance (Show n, Bounded n, Integral n, Fractional n) => Show (N n) where
  show (N coeff) =
    let halfway = maxBound / 2
     in if coeff >= halfway
          then show (negate (toInteger (maxBound - coeff) + 1))
          else show (toInteger coeff)
