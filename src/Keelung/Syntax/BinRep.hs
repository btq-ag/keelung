{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Keelung.Syntax.BinRep where

import Control.DeepSeq (NFData)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
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

--------------------------------------------------------------------------------

-- | A collection of BinReps sorted according to
--    1. their bitwidth
--    2. their corresponding variable
newtype BinReps = BinReps {unBinReps :: IntMap (IntMap BinRep)}
  deriving (Eq, Generic, NFData)

instance Serialize BinReps

instance Semigroup BinReps where
  xs <> ys = BinReps $ IntMap.unionWith (<>) (unBinReps xs) (unBinReps ys)

instance Monoid BinReps where
  mempty = BinReps IntMap.empty

instance Show BinReps where
  show binReps =
    let size' = size binReps
     in if size' == 0
          then ""
          else
            "  Binary representation constriants (" <> show size' <> "):\n"
              <> unlines
                ( map
                    (("    " <>) . show)
                    (toList binReps)
                )

fromList :: [BinRep] -> BinReps
fromList = foldl (flip insert) mempty

toList :: BinReps -> [BinRep]
toList = concatMap IntMap.elems . IntMap.elems . unBinReps

insert :: BinRep -> BinReps -> BinReps
insert binRep xs = BinReps $ IntMap.insertWith (<>) (binRepWidth binRep) (IntMap.singleton (binRepVar binRep) binRep) (unBinReps xs)

size :: BinReps -> Int
size = IntMap.foldl' (\acc set -> acc + IntMap.size set) 0 . unBinReps

lookup :: Int -> Var -> BinReps -> Maybe BinRep
lookup width var (BinReps binReps) = IntMap.lookup width binReps >>= IntMap.lookup var