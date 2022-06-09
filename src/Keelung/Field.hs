{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Keelung.Field where

import Data.Euclidean (Euclidean, GcdDomain)
import Data.Field (Field)
import Data.Field.Galois (Binary, Prime)
import Data.Semiring (Ring, Semiring)
import Data.Serialize (Serialize (..))
import GHC.Generics

--------------------------------------------------------------------------------

-- | Common Galois fields
type B64 = Binary 18446744073709551643

type GF181 = Prime 1552511030102430251236801561344621993261920897571225601

type BN128 = Prime 21888242871839275222246405745257275088548364400416034343698204186575808495617

instance Serialize B64 where
  put = put . toInteger
  get = fromInteger <$> get

instance Serialize GF181 where
  put = put . toInteger
  get = fromInteger <$> get

instance Serialize BN128 where
  put = put . toInteger
  get = fromInteger <$> get

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

--------------------------------------------------------------------------------

-- | Field types provided by the compiler
data FieldType
  = B64 -- Binary field of 64 bits
  | GF181 -- Prime field of order 181
  | BN128 -- Barreto-Naehrig
  deriving (Generic, Eq, Show)

-- | Typeclass for reflecting the field type
class AcceptedField n where
  encodeFieldType :: forall proxy. proxy n -> FieldType

instance AcceptedField B64 where
  encodeFieldType = const B64

instance AcceptedField GF181 where
  encodeFieldType = const GF181

instance AcceptedField BN128 where
  encodeFieldType = const BN128

-- | Restore the field type from an Integer
realizeAs :: Num n => FieldType -> Integer -> n
realizeAs B64 n = fromInteger n
realizeAs GF181 n = fromInteger n
realizeAs BN128 n = fromInteger n

-- | Utility function for normalizing an Integer as some field element
-- the number will be negated if it is on the "upper half" of the field
normalize :: FieldType -> Integer -> Integer
normalize B64 n = toInteger (N (fromIntegral n :: B64))
normalize GF181 n = toInteger (N (fromIntegral n :: GF181))
normalize BN128 n = toInteger (N (fromIntegral n :: BN128))