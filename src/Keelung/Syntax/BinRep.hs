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
data BinRep = BinRep
  { -- | The variable
    binRepVar :: Var,
    -- | Bit width info of the variable
    binRepBitWidth :: Int,
    -- | The starting index of the binary representation
    binRepBitsIndex :: Var,
    -- | How much the view of binary representation as been rotated
    binRepRotates :: Int
  }
  deriving (Eq, Generic, NFData)

instance Serialize BinRep

instance Show BinRep where
  show (BinRep var 1 index _) = "$" <> show var <> " = $" <> show index
  show (BinRep var 2 index r) = case r `mod` 2 of
    0 -> "$" <> show var <> " = $" <> show index <> " + 2$" <> show (index + 1)
    _ -> "$" <> show var <> " = $" <> show (index + 1) <> " + 2$" <> show index
  show (BinRep var 3 index r) = case r `mod` 3 of
    0 -> "$" <> show var <> " = $" <> show index <> " + 2$" <> show (index + 1) <> " + 4$" <> show (index + 2)
    1 -> "$" <> show var <> " = $" <> show (index + 2) <> " + 2$" <> show index <> " + 4$" <> show (index + 1)
    _ -> "$" <> show var <> " = $" <> show (index + 1) <> " + 2$" <> show (index + 2) <> " + 4$" <> show index
  show (BinRep var 4 index r) = case r `mod` 4 of
    0 -> "$" <> show var <> " = $" <> show index <> " + 2$" <> show (index + 1) <> " + 4$" <> show (index + 2) <> " + 8$" <> show (index + 3)
    1 -> "$" <> show var <> " = $" <> show (index + 3) <> " + 2$" <> show index <> " + 4$" <> show (index + 1) <> " + 8$" <> show (index + 2)
    2 -> "$" <> show var <> " = $" <> show (index + 2) <> " + 2$" <> show (index + 3) <> " + 4$" <> show index <> " + 8$" <> show (index + 1)
    _ -> "$" <> show var <> " = $" <> show (index + 1) <> " + 2$" <> show (index + 2) <> " + 4$" <> show (index + 3) <> " + 8$" <> show index
  show (BinRep var w index r) =
    let w' = r `mod` w
     in "$" <> show var <> " = $" <> show (index + w - w') <> " + 2$" <> show (index + w - w' + 1) <> " + ... + 2^" <> show (w - 1) <> "$" <> show (index + w - w' - 1)

instance Ord BinRep where
  compare (BinRep x _ _ _r1) (BinRep y _ _ _r2) = compare x y

-- compare (BinRep x _ _ r1) (BinRep y _ _ r2) = case compare r1 r2 of
--   EQ -> compare x y
--   result -> result

-- fromNumBinRep :: Int -> (Var, Var) -> BinRep
-- fromNumBinRep width (var, index) = BinRep var width index 0

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
insert binRep xs = BinReps $ IntMap.insertWith (<>) (binRepBitWidth binRep) (IntMap.singleton (binRepVar binRep) binRep) (unBinReps xs)

size :: BinReps -> Int
size = IntMap.foldl' (\acc set -> acc + IntMap.size set) 0 . unBinReps

lookup :: Int -> Var -> BinReps -> Maybe BinRep
lookup width var (BinReps binReps) = IntMap.lookup width binReps >>= IntMap.lookup var