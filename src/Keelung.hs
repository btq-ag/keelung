{-# LANGUAGE DataKinds #-}

module Keelung
  ( module Keelung.Syntax,
    module Keelung.Field,
    module Keelung.Error,
    module Keelung.Monad,
    elaborate,
    -- elaborate_,
    elaborateAndFlatten,
    generateAs,
    compile,
    compileAsR1CS,
    interpret,
  )
where

import Control.Arrow (left, right, second)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Serialize
import Data.Typeable
import Keelung.Error
import Keelung.Field
import Keelung.Monad
import Keelung.Syntax
import Keelung.Syntax.Concrete (flatten)
import qualified Keelung.Syntax.Concrete as C
import System.IO.Error
import qualified System.Info
import qualified System.Process as Process
import Control.Monad (join)

-- | Internal function for invoking the Keelung compiler on PATH
wrapper :: Serialize a => [String] -> Either String a -> IO ()
wrapper args' payload = do
  result <- findKeelungc
  case result of
    Nothing -> putStrLn "Cannot install the Keelung Compiler from Docker"
    Just (cmd, args) -> do
      Process.readProcess cmd (args ++ args') (BSC.unpack $ encode payload) >>= putStrLn

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

-- | Internal function for invoking the Keelung compiler on PATH
wrapper2 :: (Serialize a, Serialize n, Integral n) => [String] -> Either String (a, [n]) -> IO (Maybe n)
wrapper2 args' payload = do
  path <- findKeelungc
  case path of
    Nothing -> do
      putStrLn "Cannot install the Keelung Compiler from Docker"
      return Nothing
    Just (cmd, args) -> do
      let payload' = right (second (map toInteger)) payload
      blob <- Process.readProcess cmd (args ++ args') (BSC.unpack $ encode payload')
      let result = decode (BSC.pack blob)
      case join result of
        Left err -> do
          putStrLn $ "Error: " ++ err
          return Nothing
        Right x -> return x

elaborate :: Comp n (Expr kind n) -> Either String (Elaborated kind n)
elaborate prog = do
  (expr, comp') <- left show $ runComp (Computation 0 0 mempty mempty mempty mempty mempty) prog
  return $ Elaborated expr comp'

elaborateAndFlatten :: (Integral n, Typeable kind, AcceptedField n) => Comp n (Expr kind n) -> Either String C.Elaborated
elaborateAndFlatten prog = do
  (expr, comp') <- left show $ runComp (Computation 0 0 mempty mempty mempty mempty mempty) prog
  return $ flatten $ Elaborated expr comp'

generateAs :: (Serialize n, Typeable kind, Integral n, AcceptedField n) => String -> Comp n (Expr kind n) -> IO ()
generateAs filepath prog = BS.writeFile filepath $ encode (elaborateAndFlatten prog)

compile :: (Serialize n, Typeable kind, Integral n, AcceptedField n) => Comp n (Expr kind n) -> IO ()
compile prog = wrapper ["protocol", "toCS"] (elaborateAndFlatten prog)

compileAsR1CS :: (Serialize n, Typeable kind, Integral n, AcceptedField n) => Comp n (Expr kind n) -> IO ()
compileAsR1CS prog = wrapper ["protocol", "toR1CS"] (elaborateAndFlatten prog)

interpret :: (Serialize n, Typeable kind, Integral n, AcceptedField n) => Comp n (Expr kind n) -> [n] -> IO (Maybe n)
interpret prog xs = wrapper2 ["protocol", "interpret"] $ case elaborateAndFlatten prog of
  Left err -> Left err
  Right elab -> Right (elab, xs)
