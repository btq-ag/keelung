{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Errors of Keelung
module Keelung.Error where

import Control.DeepSeq (NFData)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Keelung.Heap (Addr)

--------------------------------------------------------------------------------

-- | Errors that can occur when running commands
data Error
  = DecodeError String -- Cannot decode the output from the Keelung compiler
  | CannotLocateKeelungC -- Cannot locate the Keelung compiler
  | CannotLocateProver -- Cannot locate the prover
  | CannotLocateVerifier -- Cannot locate the verifier
  | CannotLocateSnarkjs -- Cannot locate Snarkjs when needed
  | CannotReadVersionError -- Cannot read the version of the Keelung compiler
  | VersionMismatchError Int Int Int -- The version of the Keelung compiler is not supported
  | ElabError ElabError
  | CompileError String
  | SnarkjsError String
  deriving (Eq, Generic, NFData)

instance Show Error where
  show (DecodeError err) = "Decode Error: " ++ err
  show CannotLocateKeelungC = "Cannot locate the Keelung compiler"
  show CannotLocateProver = "Cannot locate the prover"
  show CannotLocateVerifier = "Cannot locate the verifier"
  show CannotLocateSnarkjs = "Cannot locate Snarkjs, please make sure it\'s installed in your $PATH."
  show CannotReadVersionError = "Cannot read the version of the Keelung compiler"
  show (VersionMismatchError major minor patch) =
    "The version of the Keelung compiler is not supported: \n"
      ++ "  expected range of version: >= v0.26.0 and < v0.27.0, but got v"
      ++ show major
      ++ "."
      ++ show minor
      ++ "."
      ++ show patch
  show (ElabError err) = "Elaboration Error: " ++ show err
  show (CompileError err) = "Compile Error: " ++ err
  show (SnarkjsError err) = "Snarkjs Error: " ++ err

instance Serialize Error

--------------------------------------------------------------------------------

-- | Errors that can occur during elaboration
data ElabError = IndexOutOfBoundsError Addr Int (IntMap Int)
  deriving (Eq, Generic, NFData)

instance Serialize ElabError

instance Show ElabError where
  show (IndexOutOfBoundsError addr i array) =
    "Index "
      ++ show i
      ++ " out of bounds for array #"
      ++ show addr
      ++ " of length "
      ++ show (IntMap.size array)
