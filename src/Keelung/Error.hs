{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Keelung.Error where

import qualified Data.IntMap as IntMap
import Keelung.Types (Addr)
import Data.IntMap (IntMap)
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Control.DeepSeq (NFData)

--------------------------------------------------------------------------------

data Error
  = DecodeError String -- Cannot decode the output from the Keelung compiler
  | InstallError -- Cannot locate the Keelung compiler
  | ElabError ElabError
  | CompileError String
  deriving (Eq, Generic, NFData)

instance Show Error where
  show (DecodeError err) = "Decode Error: " ++ err
  show InstallError = "Cannot locate the Keelung compiler"
  show (ElabError err) = "Elaboration Error: " ++ show err
  show (CompileError err) = "Compile Error: " ++ err

instance Serialize Error

--------------------------------------------------------------------------------

data ElabError
  = EmptyArrayError
  | IndexOutOfBoundsError Addr Int (IntMap Int)
  | IndexOutOfBoundsError2 Int Int 
  deriving (Eq, Generic, NFData)

instance Serialize ElabError

instance Show ElabError where
  show EmptyArrayError = "Array size must not be 0"
  show (IndexOutOfBoundsError addr i array) =
    "Index " ++ show i ++ " out of bounds for array #"
      ++ show addr
      ++ " of length "
      ++ show (IntMap.size array)
  show (IndexOutOfBoundsError2 len index) =
    "Index " ++ show index ++ " out of bounds for array of length " ++ show len

