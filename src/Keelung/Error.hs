module Keelung.Error where

import qualified Data.IntMap as IntMap
import Keelung.Types (Addr, Heap)

--------------------------------------------------------------------------------

data Error
  = DecodeError String -- Cannot decode the output from the Keelung compiler
  | InstallError -- Cannot locate the Keelung compiler
  | ElabError ElabError
  | CompileError String
  deriving (Eq)

instance Show Error where
  show (DecodeError err) = "Decode Error: " ++ err
  show InstallError = "Cannot locate the Keelung compiler"
  show (ElabError err) = "Elaboration Error: " ++ show err
  show (CompileError err) = "Compile Error: " ++ err

--------------------------------------------------------------------------------

data ElabError
  = EmptyArrayError
  | IndexOutOfBoundsError Addr Int Heap
  deriving (Eq)

instance Show ElabError where
  show EmptyArrayError = "Array size must not be 0"
  show (IndexOutOfBoundsError addr i heap) =
    "Index " ++ show i ++ " out of bounds for array "
      ++ show addr
      ++ " of length "
      ++ show (IntMap.size heap)