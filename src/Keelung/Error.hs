module Keelung.Error where

import Keelung.Types (Addr, Heap)

data Error
  = EmptyArrayError
  | IndexOutOfBoundsError Addr Int
  | UnboundArrayError Addr Int Heap
  deriving (-- | UnboundArrayError Addr Int Heap
            Eq)

instance Show Error where
  show EmptyArrayError = "Array size must not be 0"
  show (IndexOutOfBoundsError addr n) =
    "Index " ++ show n ++ " out of bounds for array " ++ show addr
  show (UnboundArrayError addr i heap) =
    "Unbound index at " ++ show i ++ " of array " ++ show addr ++ " in " ++ show heap