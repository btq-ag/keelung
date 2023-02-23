{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

-- | Field types provided by the compiler
module Keelung.Field
  ( B64,
    GF181,
    BN128,
    FieldType (..),
    realizeAs,
    normalize,
    module Keelung.Data.N,
  )
where

import Control.DeepSeq (NFData)
import Data.Field.Galois (Binary, Prime)
import Data.Serialize (Serialize (..))
import GHC.Generics (Generic)
import Keelung.Data.N

--------------------------------------------------------------------------------

-- | Binary field of 64 bits
type B64 = Binary 18446744073709551643

-- | Prime field of order 1552511030102430251236801561344621993261920897571225601
type GF181 = Prime 1552511030102430251236801561344621993261920897571225601

-- | Barreto-Naehrig curve of 128 bits
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
  = -- | Binary field of 64 bits
    B64
  | -- | Prime field of order 181
    GF181
  | -- | Barreto-Naehrig curve of 128 bits
    BN128
  deriving
    ( Generic,
      Eq,
      Show,
      NFData
    )

-- | Restore the field type from an 'Integer'
realizeAs :: Num n => FieldType -> Integer -> n
realizeAs B64 n = fromInteger n
realizeAs GF181 n = fromInteger n
realizeAs BN128 n = fromInteger n

-- | Utility function for normalizing an 'Integer' as some field element
-- the number will be negated if it is on the "upper half" of the field
normalize :: FieldType -> Integer -> Integer
normalize B64 n = toInteger (N (fromIntegral n :: B64))
normalize GF181 n = toInteger (N (fromIntegral n :: GF181))
normalize BN128 n = toInteger (N (fromIntegral n :: BN128))