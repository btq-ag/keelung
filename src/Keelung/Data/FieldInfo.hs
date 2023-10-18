{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Keelung.Data.FieldInfo (FieldInfo (..), caseFieldType) where

import Control.DeepSeq (NFData)
import Data.Field.Galois
import Data.Proxy
import GHC.Generics (Generic)
import GHC.TypeLits
import Keelung.Field (FieldType (..))
import Data.Serialize (Serialize)

-- | Runtime info data of a field
data FieldInfo = FieldInfo
  { fieldTypeData :: FieldType,
    fieldOrder :: Integer, -- size of the field
    fieldChar :: Natural,
    fieldDeg :: Int,
    fieldWidth :: Int -- the maximum bit width `w` such that `2^w <= fieldOrder`
  }
  deriving (Show, Eq, Generic, NFData)

instance Serialize FieldInfo

-- | Induction on `FieldInfo`
caseFieldType ::
  FieldType ->
  (forall n. KnownNat n => Proxy (Prime n) -> FieldInfo -> IO a) ->
  (forall n. KnownNat n => Proxy (Binary n) -> FieldInfo -> IO a) ->
  IO a
caseFieldType (Prime n) funcPrime _ = case someNatVal n of
  Just (SomeNat (_ :: Proxy n)) -> do
    let fieldNumber = asProxyTypeOf 0 (Proxy :: Proxy (Prime n))
     in funcPrime (Proxy :: Proxy (Prime n)) $
          FieldInfo
            { fieldTypeData = Prime n,
              fieldOrder = toInteger (order fieldNumber),
              fieldChar = char fieldNumber,
              fieldDeg = fromIntegral (deg fieldNumber),
              fieldWidth = floor (logBase (2 :: Double) (fromIntegral (order fieldNumber)))
            }
  Nothing -> error "[ panic ] caseFieldType: someNatVal failure"
caseFieldType (Binary n) _ funcBinary = case someNatVal n of
  Just (SomeNat (_ :: Proxy n)) ->
    let fieldNumber = asProxyTypeOf 0 (Proxy :: Proxy (Binary n))
     in funcBinary (Proxy :: Proxy (Binary n)) $
          FieldInfo
            { fieldTypeData = Binary n,
              fieldOrder = toInteger (order fieldNumber),
              fieldChar = char fieldNumber,
              fieldDeg = fromIntegral (deg fieldNumber),
              fieldWidth = floor (logBase (2 :: Double) (fromIntegral (order fieldNumber)))
            }
  Nothing -> error "[ panic ] caseFieldType: someNatVal failure"