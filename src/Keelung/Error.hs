module Keelung.Error where

import Keelung.Types (Addr, Heap)

--------------------------------------------------------------------------------

data Error
  = DecodeError String -- Cannot decode the output from the Keelung compiler
  | InstallError -- Cannot locate the Keelung compiler
  | ElabError ElabError
  | CompileError String

instance Show Error where
  show (DecodeError err) = "Decode Error: " ++ err
  show InstallError = "Cannot locate the Keelung compiler"
  show (ElabError err) = "Elaboration Error: " ++ show err
  show (CompileError err) = "Compile Error: " ++ err

--------------------------------------------------------------------------------

data ElabError
  = EmptyArrayError
  | IndexOutOfBoundsError Addr Int
  | UnboundArrayError Addr Int Heap
  deriving (-- | UnboundArrayError Addr Int Heap
            Eq)

instance Show ElabError where
  show EmptyArrayError = "Array size must not be 0"
  show (IndexOutOfBoundsError addr n) =
    "Index " ++ show n ++ " out of bounds for array " ++ show addr
  show (UnboundArrayError addr i heap) =
    "Unbound index at " ++ show i ++ " of array " ++ show addr ++ " in " ++ show heap