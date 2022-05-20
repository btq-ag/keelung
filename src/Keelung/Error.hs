module Keelung.Error where

import Keelung.Syntax (Addr, Heap)

data Error
  = EmptyArrayError
  | UnboundArrayError Addr Int Heap
  deriving Eq 

instance Show Error where
  show EmptyArrayError = "Array size must not be 0"
  show (UnboundArrayError addr i heap) =
    "Unbound index at " ++ show i ++ " of array " ++ show addr ++ " in " ++ show heap