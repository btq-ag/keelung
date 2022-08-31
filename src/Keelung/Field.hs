{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveAnyClass #-}

module Keelung.Field
  ( B64,
    GF181,
    BN128,
    FieldType (..),
    -- AcceptedField(..),
    realizeAs,
    normalize,
    module Keelung.Field.N,
  )
where

import Data.Field.Galois (Binary, Prime)
import Data.Serialize (Serialize (..))
import GHC.Generics ( Generic )
import Keelung.Field.N
import Control.DeepSeq (NFData)

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

-- | Field types provided by the compiler
data FieldType
  = B64 -- Binary field of 64 bits
  | GF181 -- Prime field of order 181
  | BN128 -- Barreto-Naehrig
  deriving (Generic, Eq, Show, NFData)

-- | Typeclass for reflecting the field type
-- class AcceptedField n where
--   encodeFieldType :: forall proxy. proxy n -> FieldType

-- instance AcceptedField B64 where
--   encodeFieldType = const B64

-- instance AcceptedField GF181 where
--   encodeFieldType = const GF181

-- instance AcceptedField BN128 where
--   encodeFieldType = const BN128

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