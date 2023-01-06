{-# LANGUAGE OverloadedStrings #-}

module Keelung
  ( module Keelung.Syntax,
    module Keelung.Field,
    module Keelung.Monad,
    module Keelung.Syntax.Bits,
    run,
    compile,
    compileO0,
    compileO2,
    compileWithOpts,
    optOptimize,
    rtsoptProf,
    rtsoptMemory,
    generate,
    verify,
    genCircuit,
    genWitness,
    interpret,
    interpret_,
    gf181,
    bn128,
    b64,
    elaborate',
    elaborate,
    Encode,
    GaloisField,
    keelungVersion,
  )
where

import Control.Arrow (left)
import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS
import Data.Field.Galois (GaloisField)
import Data.Serialize (Serialize)
import qualified Data.Serialize as Serialize
import Keelung.Constraint.R1CS (R1CS)
import Keelung.Data.Struct (Struct (..))
import Keelung.Error
import Keelung.Field
import Keelung.Monad
import Keelung.Syntax
import Keelung.Syntax.Bits
import Keelung.Syntax.Simplify
import qualified Keelung.Syntax.Typed as Typed
import qualified System.Directory as Path
import qualified System.IO.Error as IO
import qualified System.Info
import qualified System.Process as Process
import Text.Read (readMaybe)

-- | Compile a program to a 'R1CS' constraint system.
compile :: Encode t => FieldType -> Comp t -> IO (Either Error (R1CS Integer))
compile = compileWithOpts 1 [] []

compileO0 :: Encode t => FieldType -> Comp t -> IO (Either Error (R1CS Integer))
compileO0 = compileWithOpts 0 [] []

compileO2 :: Encode t => FieldType -> Comp t -> IO (Either Error (R1CS Integer))
compileO2 = compileWithOpts 2 [] []

-- The first argument speficies an optimization level.
compileWithOpts :: Encode t => Int -> [String] -> [String] -> FieldType -> Comp t -> IO (Either Error (R1CS Integer))
compileWithOpts level opts rtsopts fieldType prog = runM $ do
  elab <- liftEither (elaborate prog)
  let opts' = "protocol" : optOptimize level : opts <> [ "+RTS" ] <> rtsopts <> [ "-RTS" ]
  case fieldType of
    GF181 -> convertFieldElement (wrapper opts' (fieldType, elab) :: M (R1CS GF181))
    BN128 -> convertFieldElement (wrapper opts' (fieldType, elab) :: M (R1CS BN128))
    B64 -> convertFieldElement (wrapper opts' (fieldType, elab) :: M (R1CS B64))

-- Common options
optOptimize :: Int -> String
optOptimize i = "O" <> show i

rtsoptProf :: [String]
rtsoptProf = ["+RTS", "-p", "-RTS"]

-- Memory size in GB for RTS options -M, -H and in MB for -A
-- Try to increase if keelungc produces segmentation fault.
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/runtime_control.html
rtsoptMemory :: Int -> Int -> Int -> [String]
rtsoptMemory m h a = ["-M" <> show m <> "G", "-H" <> show h <> "G", "-A" <> show a <> "M"]

--------------------------------------------------------------------------------

-- | Generate a proof
generate_ :: (Serialize n, Integral n, Encode t) => FieldType -> Comp t -> [n] -> IO (Either Error (FilePath, String))
generate_ fieldType prog inputs' = runM $ do
  exists <- checkCmd "instrument_aurora_snark_proof"
  _ <- genCircuit fieldType prog
  _ <- genWitness_ fieldType prog inputs'
  proofPath <- lift $ Path.makeAbsolute "proof"
  genParameters
  if exists
    then lift $ do
      let arguments =
            [ "--r1cs_filepath",
              "circuit.jsonl",
              "--input_filepath",
              "witness.jsonl",
              "--parameter_filepath",
              "parameter.json",
              "--output_filepath",
              proofPath
            ]
      msg <- Process.readProcess "instrument_aurora_snark_proof" arguments mempty
      Path.removeFile "circuit.jsonl"
      Path.removeFile "witness.jsonl"
      Path.removeFile "parameter.json"
      return (proofPath, msg)
    else throwError CannotLocateProver

generate :: Encode t => FieldType -> Comp t -> [Integer] -> IO ()
generate fieldType prog inputs' = do
  result <- generate_ fieldType prog inputs'
  case result of
    Left err -> print err
    Right (_, msg) -> putStr msg

-- | Generate and verify a proof
verify_ :: (Serialize n, Integral n, Encode t) => FieldType -> Comp t -> [n] -> IO (Either Error String)
verify_ fieldType prog inputs' = runM $ do
  (proofPath, _) <- liftEitherT $ generate_ fieldType prog inputs'
  exists <- checkCmd "instrument_aurora_snark_verify"
  _ <- genCircuit fieldType prog
  _ <- genWitness_ fieldType prog inputs'
  genParameters
  if exists
    then lift $ do
      let arguments =
            [ "--r1cs_filepath",
              "circuit.jsonl",
              "--input_filepath",
              "witness.jsonl",
              "--parameter_filepath",
              "parameter.json",
              "--proof_filepath",
              proofPath
            ]
      msg <- Process.readProcess "instrument_aurora_snark_verify" arguments mempty
      Path.removeFile "circuit.jsonl"
      Path.removeFile "witness.jsonl"
      Path.removeFile "parameter.json"
      return msg
    else throwError CannotLocateVerifier

verify :: Encode t => FieldType -> Comp t -> [Integer] -> IO ()
verify fieldType prog inputs' = do
  result <- verify_ fieldType prog inputs'
  case result of
    Left err -> print err
    Right msg -> putStr msg

-- | Compile a program as R1CS and write it to circuit.jsonl.
genCircuit :: Encode t => FieldType -> Comp t -> M (R1CS Integer)
genCircuit fieldType prog = do
  elab <- liftEither (elaborate prog)
  case fieldType of
    GF181 -> convertFieldElement (wrapper ["protocol", "toJSON"] (fieldType, elab) :: M (R1CS GF181))
    BN128 -> convertFieldElement (wrapper ["protocol", "toJSON"] (fieldType, elab) :: M (R1CS BN128))
    B64 -> convertFieldElement (wrapper ["protocol", "toJSON"] (fieldType, elab) :: M (R1CS B64))

-- | Generate witnesses for a program with inputs and write them to witness.jsonl.
genWitness_ :: (Serialize n, Integral n, Encode t) => FieldType -> Comp t -> [n] -> M [n]
genWitness_ fieldType prog xs = do
  elab <- liftEither (elaborate prog)
  wrapper ["protocol", "genWitness"] (fieldType, elab, map toInteger xs)

genParameters :: M ()
genParameters = lift $ BS.writeFile "parameter.json" "{\"security_level\": 128, \"heuristic_ldt_reducer_soundness\": true, \"heuristic_fri_soundness\": true, \"bcs_hash_type\": \"blake2b_type\", \"make_zk\": false, \"parallel\": true, \"field_size\": 181, \"is_multiplicative\": true}"

genWitness :: Encode t => FieldType -> Comp t -> [Integer] -> IO [Integer]
genWitness fieldType prog xs = runM (genWitness_ fieldType prog xs) >>= printErrorInstead

--------------------------------------------------------------------------------

interpret_ :: (Serialize n, Integral n, Encode t) => FieldType -> Comp t -> [n] -> IO (Either Error [n])
interpret_ fieldType prog xs = runM $ do
  elab <- liftEither (elaborate prog)
  wrapper ["protocol", "interpret"] (fieldType, elab, map toInteger xs)

printErrorInstead :: Show e => Either e [a] -> IO [a]
printErrorInstead (Left err) = do
  print err
  return []
printErrorInstead (Right values) = return values

-- | Interpret a program with private and public inputs
run :: Encode t => Comp t -> [Integer] -> [Integer] -> IO [Integer]
run prog private public = interpret_ GF181 prog (private ++ public) >>= printErrorInstead

-- | Interpret a program with inputs
interpret :: Encode t => FieldType -> Comp t -> [Integer] -> IO [Integer]
interpret fieldType prog xs = interpret_ fieldType prog xs >>= printErrorInstead

-- | A specialized version of 'interpret' that outputs numbers as 'N GF181'
gf181 :: Encode t => Comp t -> [GF181] -> IO [N GF181]
gf181 prog xs = map N <$> (interpret_ GF181 prog xs >>= printErrorInstead)

-- | A specialized version of 'interpret' that outputs numbers as 'N B64'
b64 :: Encode t => Comp t -> [B64] -> IO [N B64]
b64 prog xs = map N <$> (interpret_ B64 prog xs >>= printErrorInstead)

-- | A specialized version of 'interpret' that outputs numbers as 'N BN128'
bn128 :: Encode t => Comp t -> [BN128] -> IO [N BN128]
bn128 prog xs = map N <$> (interpret_ BN128 prog xs >>= printErrorInstead)

--------------------------------------------------------------------------------

-- | Elaborate a program to the Kinded Syntax
elaborate' :: Comp t -> Either Error (Elaborated t)
elaborate' prog = do
  (expr, comp') <- left ElabError $ runComp emptyComputation prog
  return $ Elaborated expr comp'

-- | Elaborate a program and convert it to the Typed Syntax
elaborate :: Encode t => Comp t -> Either Error Typed.Elaborated
elaborate prog = encodeElaborated <$> elaborate' prog
  where
    encodeElaborated :: Encode t => Elaborated t -> Typed.Elaborated
    encodeElaborated (Elaborated expr comp) = runHeapM (compHeap comp) $ do
      let Computation counters _addrSize _heap eb assertions = comp
       in Typed.Elaborated
            <$> encode expr
            <*> ( Typed.Computation
                    counters
                    <$> (Struct <$> mapM encode' (structF eb) <*> mapM encode' (structB eb) <*> pure (structU eb))
                    <*> pure mempty
                    <*> mapM encode assertions
                )

--------------------------------------------------------------------------------

-- | Internal function for handling data serialization
wrapper :: (Serialize a, Serialize b) => [String] -> a -> M b
wrapper args' payload = do
  (cmd, args) <- findKeelungc
  version <- readKeelungVersion cmd args
  checkKeelungVersion version
  blob <- lift $ Process.readProcess cmd (args ++ args') (BS.unpack $ Serialize.encode payload)
  let result = Serialize.decode (BS.pack blob)
  case result of
    Left err -> throwError (DecodeError err)
    Right (Left err) -> throwError (CompileError err)
    Right (Right x) -> return x

-- | Locate the Keelung compiler
--      1. see if "keelungc" is in PATH
--      2. if not, try to run "docker run banacorn/keelung"
--   Returns the command and arguments to run when found
findKeelungc :: M (String, [String])
findKeelungc = do
  keelungcExists <- checkCmd "keelungc"
  if keelungcExists
    then return ("keelungc", [])
    else do
      dockerExists <- checkCmd "docker"
      if dockerExists
        then -- insert "--platform=linux/amd64" when we are not on a x86 machine
        case System.Info.arch of
          "x86_64" -> return ("docker", ["run", "-i", "banacorn/keelung"])
          _ -> return ("docker", ["run", "-i", "--platform=linux/amd64", "banacorn/keelung"])
        else throwError CannotLocateKeelungC

-- | Check the version of the Keelung compiler
readKeelungVersion :: FilePath -> [String] -> M (Int, Int, Int)
readKeelungVersion cmd args = do
  -- trying to read version with `keelungc --version`
  rawString <-
    catchIOError
      CannotReadVersionError
      (Process.readProcess cmd (args ++ ["--version"]) mempty)
  -- parse see if the version number is well-formed
  let parseResult = case splitAt 9 rawString of
        ("Keelung v", versionString) -> parseVersion versionString
        _ -> Nothing
  -- throws CannotReadVersionError if it's not well-formed
  case parseResult of
    Nothing -> throwError CannotReadVersionError
    Just x -> return x
  where
    parseVersion :: String -> Maybe (Int, Int, Int)
    parseVersion versionString = do
      (major, minor, patch) <- case span (/= '.') versionString of
        (m, '.' : rest) -> case span (/= '.') rest of
          (n, '.' : p) -> Just (m, n, p)
          _ -> Nothing
        _ -> Nothing
      (,,) <$> readMaybe major <*> readMaybe minor <*> readMaybe patch

checkKeelungVersion :: (Int, Int, Int) -> M ()
checkKeelungVersion (major, minor, patch) = do
  if major == 0 && minor >= 8 && minor < 9 && patch >= 2
    then return ()
    else throwError (VersionMismatchError major minor patch)

--------------------------------------------------------------------------------

-- | Check if a command exists
checkCmd :: String -> M Bool
checkCmd cmd =
  lift $
    IO.catchIOError
      (Process.readProcess whichCmd [cmd] mempty >> return True)
      (\_ -> return False)
  where
    -- decide the command for locating executables
    whichCmd :: String
    whichCmd = case System.Info.os of
      "mingw32" -> "where" -- Windows uses "where"
      _ -> "which" -- Unix uses "which"

--------------------------------------------------------------------------------

-- | The version of Keelung is a triple of three numbers, we're not going full semver yet
keelungVersion_ :: (Int, Int, Int)
keelungVersion_ = (0, 8, 2)

-- | String of Keelung version exposed to the user
keelungVersion :: String
keelungVersion = let (major, minor, patch) = keelungVersion_ in show major ++ "." ++ show minor ++ "." ++ show patch

--------------------------------------------------------------------------------

type M = ExceptT Error IO

runM :: M a -> IO (Either Error a)
runM = runExceptT

liftEitherT :: IO (Either Error a) -> M a
liftEitherT f = do
  result <- lift f
  case result of
    Left err -> throwError err
    Right x -> return x

-- | Handle 'IO' Exceptions in the 'M' Monad
catchIOError :: Error -> IO a -> M a
catchIOError err f = lift (IO.catchIOError (Right <$> f) (const (return (Left err)))) >>= liftEither

-- | Prettify and convert all field elements to 'Integer' in a 'R1CS'
convertFieldElement :: (GaloisField a, Integral a) => M (R1CS a) -> M (R1CS Integer)
convertFieldElement = fmap (fmap (toInteger . N))
