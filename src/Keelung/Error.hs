{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Keelung.Error where

import Control.DeepSeq (NFData)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Keelung.Types (Addr)

--------------------------------------------------------------------------------

data Error
  = DecodeError String -- Cannot decode the output from the Keelung compiler
  | CannotLocateKeelungC -- Cannot locate the Keelung compiler
  | CannotLocateProver -- Cannot locate the prover
  | CannotLocateVerifier -- Cannot locate the verifier
  | CannotReadVersionError -- Cannot read the version of the Keelung compiler
  | VersionMismatchError Int Int Int -- The version of the Keelung compiler is not supported
  | ElabError ElabError
  | CompileError String
  deriving (Eq, Generic, NFData)

instance Show Error where
  show (DecodeError err) = "Decode Error: " ++ err
  show CannotLocateKeelungC = "Cannot locate the Keelung compiler"
  show CannotLocateProver = "Cannot locate the prover"
  show CannotLocateVerifier = "Cannot locate the verifier"
  show CannotReadVersionError = "Cannot read the version of the Keelung compiler"
  show (VersionMismatchError major minor patch) =
    "The version of the Keelung compiler is not supported: \n"
      ++ "  expected range of version: >= v0.8.1 and < v0.9.0, but got v"
      ++ show major
      ++ "."
      ++ show minor
      ++ "."
      ++ show patch
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
