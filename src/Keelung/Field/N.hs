{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Keelung.Field.N where

import Data.Euclidean (Euclidean, GcdDomain)
import Data.Field (Field)
import Data.Field.Galois (GaloisField (..))
import Data.Semiring (Ring, Semiring)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Test.QuickCheck (Arbitrary)
import Data.Group (Group)
import System.Random (Random)
import Text.PrettyPrint.Leijen.Text (Pretty)

-- import Data.Field.Galois (GaloisField(..))

--------------------------------------------------------------------------------

-- | Data type for displaying field elements nicely
-- Elements in the second half of the field are represented as negative numbers
newtype N a = N {unN :: a}
  deriving (Eq, Ord, Generic, NFData)
  
instance Serialize a => Serialize (N a)

deriving instance Bounded n => Bounded (N n)

deriving instance Arbitrary n => Arbitrary (N n)

deriving instance Field n => Field (N n)

deriving instance Group n => Group (N n)
deriving instance Monoid n => Monoid (N n)
deriving instance Semigroup n => Semigroup (N n)
deriving instance Random n => Random (N n)
deriving instance Pretty n => Pretty (N n)
deriving instance (GaloisField n, Integral n) => GaloisField (N n)

deriving instance Euclidean n => Euclidean (N n)

deriving instance Ring n => Ring (N n)

deriving instance GcdDomain n => GcdDomain (N n)

deriving instance Semiring n => Semiring (N n)

deriving instance Fractional n => Fractional (N n)

deriving instance Num n => Num (N n)

deriving instance Enum n => Enum (N n)

deriving instance Real n => Real (N n)

instance (GaloisField n, Integral n) => Integral (N n) where
  quotRem n m = (N q, N r)
    where
      (q, r) = quotRem (unN n) (unN m)
  toInteger (N x) =
    let halfway = fromIntegral (order x `div` 2)
     in if x >= halfway
          then negate (toInteger (order x) - toInteger x)
          else toInteger x

instance (GaloisField n, Integral n) => Show (N n) where
  show = show . toInteger