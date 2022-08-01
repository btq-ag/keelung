{-# LANGUAGE DataKinds #-}

module Keelung
  ( module Keelung.Syntax,
    module Keelung.Field,
    -- module Keelung.Error,
    module Keelung.Monad,
    Kind (..),
    Compilable (..),
    GaloisField,
  )
where

import Control.Arrow (left)
import qualified Data.ByteString.Char8 as BSC
import Data.Field.Galois (GaloisField)
import Data.Serialize
import Keelung.Constraint.R1CS (R1CS)
import Keelung.Field
import Keelung.Monad
import Keelung.Syntax
import Keelung.Syntax.Concrete (flatten)
import qualified Keelung.Syntax.Concrete as C
import Keelung.Types
import System.IO.Error
import qualified System.Info
import qualified System.Process as Process
import Keelung.Error (ElabError)

-- | Internal function for handling data serialization
wrapper :: (Serialize a, Serialize b) => [String] -> Either String a -> IO (Either Error b)
wrapper args' payload = do
  path <- findKeelungc
  case path of
    Nothing -> return $ Left InstallError
    Just (cmd, args) -> do
      blob <- Process.readProcess cmd (args ++ args') (BSC.unpack $ encode payload)
      let result = decode (BSC.pack blob)
      case result of
        Left err -> return $ Left $ DecodeError err
        Right (Left err) -> return $ Left $ CompileError err
        Right (Right x) -> return $ Right x

-- | Locate the Keelung compiler
--      1. see if "keelungc" is in PATH
--      2. if not, try to run "docker run banacorn/keelung"
findKeelungc :: IO (Maybe (String, [String]))
findKeelungc = do
  keelungcExists <- checkCmd "keelungc"
  if keelungcExists
    then return $ Just ("keelungc", [])
    else do
      dockerExists <- checkCmd "docker"
      if dockerExists
        then -- insert "--platform=linux/amd64" when we are not on a x86 machine
        case System.Info.arch of
          "x86_64" -> return $ Just ("docker", ["run", "-i", "banacorn/keelung"])
          _ -> return $ Just ("docker", ["run", "-i", "--platform=linux/amd64", "banacorn/keelung"])
        else return Nothing
  where
    -- decide the command for locating executables
    whichCmd :: String
    whichCmd = case System.Info.os of
      "mingw32" -> "where" -- Windows uses "where"
      _ -> "which" -- Unix uses "which"

    -- check if a command exists
    checkCmd :: String -> IO Bool
    checkCmd cmd =
      catchIOError
        (Process.readProcess whichCmd [cmd] mempty >> return True)
        (\_ -> return False)

class Compilable t where
  elaborate :: Comp n (Val t n) -> Either Error (Elaborated t n)
  elaborate prog = do
    (expr, comp') <- left ElabError $ runComp (Computation 0 0 mempty mempty mempty mempty mempty) prog
    return $ Elaborated expr comp'

  elaborateAndFlatten :: (Integral n, AcceptedField n) => Comp n (Val t n) -> Either String C.Elaborated

  compile :: (Serialize n, Integral n, AcceptedField n) => Comp n (Val t n) -> IO (Either Error (R1CS n))
  compile prog = wrapper ["protocol", "toR1CS"] (elaborateAndFlatten prog)

  interpret :: (Serialize n, Integral n, AcceptedField n) => Comp n (Val t n) -> [n] -> IO (Either Error (Maybe n))
  interpret prog xs = wrapper ["protocol", "interpret"] $ case elaborateAndFlatten prog of
    Left err -> Left err
    Right elab -> Right (elab, map toInteger xs)

instance Compilable 'Bool where
  elaborateAndFlatten prog = do
    (expr, comp') <- left show $ runComp (Computation 0 0 mempty mempty mempty mempty mempty) prog
    return $ flatten $ Elaborated expr comp'

instance Compilable 'Num where
  elaborateAndFlatten prog = do
    (expr, comp') <- left show $ runComp (Computation 0 0 mempty mempty mempty mempty mempty) prog
    return $ flatten $ Elaborated expr comp'

instance Compilable 'Unit where
  elaborateAndFlatten prog = do
    (expr, comp') <- left show $ runComp (Computation 0 0 mempty mempty mempty mempty mempty) prog
    return $ flatten $ Elaborated expr comp'

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