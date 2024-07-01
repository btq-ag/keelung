{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_HADDOCK hide #-}

module Keelung.Data.N (N (..), isPositive) where

import Control.DeepSeq (NFData)
import Data.Euclidean (Euclidean, Field, GcdDomain)
import Data.Field.Galois (GaloisField (..))
import Data.Group (Group)
import Data.Semiring (Ring, Semiring)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import System.Random (Random)
import Test.QuickCheck (Arbitrary)
import Text.PrettyPrint.Leijen.Text (Pretty)

--------------------------------------------------------------------------------

-- | Data type for displaying field elements nicely
-- Elements in the second half of the field are represented as negative numbers
newtype N a = N {unN :: a}
  deriving (Eq, Ord, Generic, NFData)

instance Serialize a => Serialize (N a)

deriving instance Bounded n => Bounded (N n)

deriving instance Group n => Group (N n)

deriving instance Field n => Field (N n)

deriving instance Monoid n => Monoid (N n)

deriving instance Semigroup n => Semigroup (N n)

deriving instance Euclidean n => Euclidean (N n)

deriving instance Ring n => Ring (N n)

deriving instance GcdDomain n => GcdDomain (N n)

deriving instance Semiring n => Semiring (N n)

deriving instance Arbitrary n => Arbitrary (N n)

deriving instance Pretty n => Pretty (N n)

deriving instance Random n => Random (N n)

deriving instance (GaloisField n, Integral n) => GaloisField (N n)

deriving instance Fractional n => Fractional (N n)

deriving instance Num n => Num (N n)

deriving instance Enum n => Enum (N n)

deriving instance Real n => Real (N n)

instance (GaloisField n, Integral n) => Integral (N n) where
  quotRem n m = (N q, N r)
    where
      (q, r) = quotRem (unN n) (unN m)
  toInteger (N x) =
    -- if the characteristic is 2, then treat the field as a binary field
    if char x == 2
      then asBinaryField x
      else asPrimeField x
    where
      asPrimeField n
        | isPositive n = toInteger n
        | otherwise = negate (toInteger (order n) - toInteger n)
      asBinaryField = toInteger

instance (GaloisField n, Integral n) => Show (N n) where
  show = show . toInteger

-- | Returns true if the given element is in the first half of the field
isPositive :: (GaloisField n, Integral n) => n -> Bool
isPositive x = x < halfway
  where
    halfway = fromIntegral (order x `div` 2)