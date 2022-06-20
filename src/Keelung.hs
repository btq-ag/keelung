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

import Control.Arrow (left)
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

-- | Internal function for invoking the Keelung compiler on PATH
wrapper :: Serialize a => [String] -> a -> IO ()
wrapper args' payload = do
  result <- findKeelungc
  case result of
    Nothing -> putStrLn "Cannot find the Keelung compiler"
    Just (cmd, args) -> do
      print ("PATH", cmd, args ++ args')
      Process.readProcess cmd (args ++ args') (BSC.unpack $ encode payload) >>= putStrLn
  where
    findKeelungc :: IO (Maybe (String, [String]))
    findKeelungc = do
      keelungcExists <- checkCmd "keelungc"
      if keelungcExists
        then return $ Just ("keelungc", [])
        else do
          keelungExists <- checkCmd "docker"
          if keelungExists
            then return $ Just ("docker", ["run", "banacorn/keelung"])
            else return Nothing

    -- | decide the command for locating executables
    whichCmd :: String
    whichCmd = case System.Info.os of
      "mingw32" -> "where" -- Windows uses "where"
      _ -> "which" -- Unix uses "which"

    -- | check if a command exists
    checkCmd :: String -> IO Bool
    checkCmd cmd =
      catchIOError
        (Process.readProcess whichCmd [cmd] mempty >> return True)
        (\_ -> return False)

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

interpret :: (Serialize n, Typeable kind, Integral n, AcceptedField n) => Comp n (Expr kind n) -> [n] -> IO ()
interpret prog inputs = wrapper ["protocol", "interpret"] (elaborateAndFlatten prog, map toInteger inputs)
