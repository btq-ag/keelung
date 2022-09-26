module Keelung
  ( module Keelung.Syntax,
    module Keelung.Field,
    module Keelung.Monad,
    compile,
    compileO0,
    compileO2,
    interpret,
    interpret_,
    gf181,
    bn128,
    b64,
    elaborate,
    Kind (..),
    Elaborable(..),
    Simplify,
    GaloisField,
  )
where

import qualified Data.ByteString.Char8 as BSC
import Data.Field.Galois (GaloisField)
import Data.Serialize
import Keelung.Constraint.R1CS (R1CS)
import Keelung.Error
import Keelung.Field
import Keelung.Monad
import Keelung.Syntax
import Keelung.Syntax.Simplify (Simplify, simplify)
import qualified Keelung.Syntax.Typed as C
import Keelung.Types
import System.IO.Error
import qualified System.Info
import qualified System.Process as Process

-- | Compile a program to a 'R1CS' constraint system.
compile :: (Elaborable t, Simplify t) => FieldType -> Comp t -> IO (Either Error (R1CS Integer))
compile fieldType prog = case elaborate prog of
  Left err -> return $ Left (ElabError err)
  Right elab ->
    case fieldType of
      GF181 -> fmap (fmap (toInteger . N)) <$> (wrapper ["protocol", "O1"] (fieldType, elab) :: IO (Either Error (R1CS GF181)))
      BN128 -> fmap (fmap (toInteger . N)) <$> (wrapper ["protocol", "O1"] (fieldType, elab) :: IO (Either Error (R1CS BN128)))
      B64 -> fmap (fmap (toInteger . N)) <$> (wrapper ["protocol", "O1"] (fieldType, elab) :: IO (Either Error (R1CS B64)))

compileO0 :: (Elaborable t, Simplify t) => FieldType -> Comp t -> IO (Either Error (R1CS Integer))
compileO0 fieldType prog = case elaborate prog of
  Left err -> return $ Left (ElabError err)
  Right elab ->
    case fieldType of
      GF181 -> fmap (fmap (toInteger . N)) <$> (wrapper ["protocol", "O0"] (fieldType, elab) :: IO (Either Error (R1CS GF181)))
      BN128 -> fmap (fmap (toInteger . N)) <$> (wrapper ["protocol", "O0"] (fieldType, elab) :: IO (Either Error (R1CS BN128)))
      B64 -> fmap (fmap (toInteger . N)) <$> (wrapper ["protocol", "O0"] (fieldType, elab) :: IO (Either Error (R1CS B64)))

compileO2 :: (Elaborable t, Simplify t) => FieldType -> Comp t -> IO (Either Error (R1CS Integer))
compileO2 fieldType prog = case elaborate prog of
  Left err -> return $ Left (ElabError err)
  Right elab ->
    case fieldType of
      GF181 -> fmap (fmap (toInteger . N)) <$> (wrapper ["protocol", "O2"] (fieldType, elab) :: IO (Either Error (R1CS GF181)))
      BN128 -> fmap (fmap (toInteger . N)) <$> (wrapper ["protocol", "O2"] (fieldType, elab) :: IO (Either Error (R1CS BN128)))
      B64 -> fmap (fmap (toInteger . N)) <$> (wrapper ["protocol", "O2"] (fieldType, elab) :: IO (Either Error (R1CS B64)))

--------------------------------------------------------------------------------

interpret_ :: (Serialize n, Integral n, Elaborable t, Simplify t) => FieldType -> Comp t -> [n] -> IO (Either Error [n])
interpret_ fieldType prog xs = case elaborate prog of
  Left err -> return $ Left (ElabError err)
  Right elab -> wrapper ["protocol", "interpret"] (fieldType, elab, map toInteger xs)

printErrorInstead :: Show e => Either e [a] -> IO [a]
printErrorInstead (Left err) = do
  print err
  return []
printErrorInstead (Right values) = return values

-- | Interpret a program with inputs
interpret :: (Elaborable t, Simplify t) => FieldType -> Comp t -> [Integer] -> IO [Integer]
interpret fieldType prog xs = interpret_ fieldType prog xs >>= printErrorInstead

-- | A specialized version of 'interpret' that outputs numbers as 'N GF181'
gf181 :: (Elaborable t, Simplify t) => Comp t -> [GF181] -> IO [N GF181]
gf181 prog xs = map N <$> (interpret_ GF181 prog xs >>= printErrorInstead)

-- | A specialized version of 'interpret' that outputs numbers as 'N B64'
b64 :: (Elaborable t, Simplify t) => Comp t -> [B64] -> IO [N B64]
b64 prog xs = map N <$> (interpret_ B64 prog xs >>= printErrorInstead)

-- | A specialized version of 'interpret' that outputs numbers as 'N BN128'
bn128 :: (Elaborable t, Simplify t) => Comp t -> [BN128] -> IO [N BN128]
bn128 prog xs = map N <$> (interpret_ BN128 prog xs >>= printErrorInstead)

--------------------------------------------------------------------------------

-- | Elaborate a program to the Kinded Syntax
class Elaborable t where
  elaborate' :: Comp t -> Either ElabError (Elaborated t)

instance Elaborable Number where 
  elaborate' prog = do
    (expr, comp') <- runComp emptyComputation prog
    return $ ElaboratedNum expr comp'

instance Elaborable Boolean where 
  elaborate' prog = do
    (expr, comp') <- runComp emptyComputation prog
    return $ ElaboratedBool expr comp'

instance Elaborable Unit where 
  elaborate' prog = do
    (expr, comp') <- runComp emptyComputation prog
    return $ ElaboratedUnit expr comp'

instance Elaborable t => Elaborable (Arr t) where 
  elaborate' prog = do
    (expr, comp') <- runComp emptyComputation prog
    return $ ElaboratedArray expr comp'

-- -- | Elaborate a program as the Kinded Syntax
-- elaborateOnly :: Comp t -> Either ElabError (Elaborated t)
-- elaborateOnly prog = do
--   (expr, comp') <- runComp emptyComputation prog
--   return $ Elaborated expr comp'

-- | Elaborate a program and simplify it to the Typed Syntax
elaborate :: (Elaborable t, Simplify t) => Comp t -> Either ElabError C.Elaborated
elaborate prog = simplify <$> elaborate' prog 

--------------------------------------------------------------------------------

-- | Internal function for handling data serialization
wrapper :: (Serialize a, Serialize b) => [String] -> a -> IO (Either Error b)
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
