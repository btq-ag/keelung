{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Keelung.Data.BinRep where

import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Keelung.Types (Var)

--------------------------------------------------------------------------------

-- | Relation between a variable and binary representation
--   $var = bit₀ + 2bit₁ + 4bit₂ + 8bit₃ + ... + 2^(n-1)bitₙ
data BinRep = BinRep
  { -- | The variable
    binRepVar :: Var,
    -- | Total number of bits
    binRepWidth :: Int,
    -- | The starting index of the bits
    binRepBitStart :: Var
  }
  deriving (Eq, Generic, NFData)

instance Serialize BinRep

instance Show BinRep where
  show (BinRep var 1 index) = "$" <> show var <> " = $" <> show index
  show (BinRep var 2 index) = "$" <> show var <> " = $" <> show index <> " + 2$" <> show (index + 1)
  show (BinRep var 3 index) = "$" <> show var <> " = $" <> show index <> " + 2$" <> show (index + 1) <> " + 4$" <> show (index + 2)
  show (BinRep var 4 index) = "$" <> show var <> " = $" <> show index <> " + 2$" <> show (index + 1) <> " + 4$" <> show (index + 2) <> " + 8$" <> show (index + 3)
  show (BinRep var w index) = "$" <> show var <> " = $" <> show index <> " + 2$" <> show (index + 1) <> " + ... + 2^" <> show (w - 1) <> "$" <> show (index + w - 1)

instance Ord BinRep where
  compare (BinRep x _ _) (BinRep y _ _) = compare x y