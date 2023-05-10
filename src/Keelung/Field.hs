{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Keelung.Field
  ( B64,
    GF181,
    BN128,
    FieldType (..),
    module Keelung.Data.N,
    gf181,
    b64,
    bn128,
  )
where

import Control.DeepSeq (NFData)
import Data.Field.Galois (Binary, Prime)
import Data.Serialize (Serialize (..))
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat)
import Keelung.Data.N

--------------------------------------------------------------------------------

-- | Shorthands for some common field types

-- | Binary field of 64 bits
type B64 = Binary 18446744073709551643

-- | Prime field of order 1552511030102430251236801561344621993261920897571225601
type GF181 = Prime 1552511030102430251236801561344621993261920897571225601

-- | Barreto-Naehrig curve of 128 bits
type BN128 = Prime 21888242871839275222246405745257275088548364400416034343698204186575808495617

instance KnownNat n => Serialize (Binary n) where
  put = put . toInteger
  get = fromInteger <$> get

instance KnownNat n => Serialize (Prime n) where
  put = put . toInteger
  get = fromInteger <$> get

--------------------------------------------------------------------------------

-- | Runtime data for specifying field types
data FieldType
  = -- | Binary fields
    Binary Integer
  | -- | Prime fields
    Prime Integer
  deriving (Generic, Eq, Show, NFData, Serialize)

--------------------------------------------------------------------------------
-- Smart constructors for `FieldType`

-- | Prime field of order 181
gf181 :: FieldType
gf181 = Prime 1552511030102430251236801561344621993261920897571225601

-- | Binary field of 64 bits
b64 :: FieldType
b64 = Binary 18446744073709551643

-- | For Barreto-Naehrig curve of 128 bits
bn128 :: FieldType
bn128 = Prime 21888242871839275222246405745257275088548364400416034343698204186575808495617