{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Keelung.Data.Struct where

import Control.DeepSeq (NFData)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Keelung.Types (Width)

--------------------------------------------------------------------------------

-- | Data structure for data associated with different primitive datatypes
data Struct f b u = Struct
  { structF :: f,
    structB :: b,
    structU :: IntMap u
  }
  deriving (Eq, Show, NFData, Generic)

instance (Serialize f, Serialize b, Serialize u) => Serialize (Struct f b u)

instance (Semigroup f, Semigroup b, Semigroup u) => Semigroup (Struct f b u) where
  Struct f1 b1 u1 <> Struct f2 b2 u2 = Struct (f1 <> f2) (b1 <> b2) (u1 <> u2)

instance (Monoid f, Monoid b, Monoid u) => Monoid (Struct f b u) where
  mempty = Struct mempty mempty mempty

updateF :: (x -> y) -> Struct x b u -> Struct y b u
updateF func (Struct f b u) = Struct (func f) b u

updateB :: (x -> y) -> Struct f x u -> Struct f y u
updateB func (Struct f b u) = Struct f (func b) u

updateU :: Width -> (x -> x) -> Struct f b x -> Struct f b x
updateU w func (Struct f b u) = Struct f b $ IntMap.adjust func w u

empty :: (Monoid f, Eq f, Eq b, Monoid b) => Struct f b u -> Bool
empty (Struct f b u) = f == mempty && b == mempty && IntMap.null u
