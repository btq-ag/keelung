{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Keelung is a DSL for building zero-knowledge proofs
module Keelung
  ( module Keelung.Syntax,
    module Keelung.Field,
    module Keelung.Heap,
    module Keelung.Monad,
    module Keelung.Data.Bits,
    module Keelung.CircuitFormat,
    keelung,
    -- interpret
    interpret,
    --interpretData,
    interpretEither,
    -- compile
    compile,
    compileO0,
    compileWithOpts,
    rtsoptProf,
    rtsoptMemory,
    -- solve R1CS
    solveOutput,
    --solveOutputData,
    solveOutputEither,
    -- witness generation
    witness,
    witness',
    -- proof generation
    prove,
    prove',
    -- proof verification
    verify,
    verify',
    -- genCircuit,
    genCircuitBin,
    genWtns,
    -- genCircuitDefault,
    genInputs,
    genInputsDefault,
    Encode,
    GaloisField,
    keelungVersion,
    proveData,
    interpretData,
    interpretDataEither,
    solveOutputDataEither,
    solveOutputData,
    stringToJsonInputs,
    jsonFileInputs,
    jsonInputs,
    SnarkjsBackend(..),
    setupSnarkjsPtau,
    checkWtnsBySnarkjs,
    genZkeyBySnarkjs,
    proveBySnarkjs,
  )
where

import Control.Monad.Except
import Data.ByteString.Char8 qualified as BS
import Data.Field.Galois (GaloisField)
import Data.List (intercalate)
import Data.Serialize (Serialize)
import Data.Serialize qualified as Serialize
import Data.String (IsString (fromString))
import Keelung.Constraint.R1CS (R1CS)
import Keelung.Data.Bits
import Keelung.Error
import Keelung.Field
import Keelung.Heap
import Keelung.Monad
import Keelung.Options
import Keelung.Syntax
import Keelung.Syntax.Encode
import Keelung.CircuitFormat
import System.Directory qualified as Path
--import System.FilePath qualified as FP
import System.IO.Error qualified as IO
import System.Info qualified
import System.Process qualified as Process
import Text.Read (readMaybe)
import Data.Aeson
import GHC.IO.Exception (ExitCode(..))

-- | IMPORTANT: The compatibale compiler version of this library, Make sure it's updated and matched accordingly.
keelungCompilerVersion :: (Int, Int, Int)
keelungCompilerVersion = (0, 26, 1)

-- | The version of this library in String
keelungVersion :: String
keelungVersion = (\(a,b,c) -> intercalate "." [show a, show b, show c]) keelungCompilerVersion

--------------------------------------------------------------------------------

-- | Entry point for the Keelung command line interface
keelung :: (Encode t) => Comp t -> IO ()
keelung program = do
  -- replace with beefier option parser
  options <- getOptions
  case options of
    Compile fieldType -> compile fieldType program >>= printResult
    Interpret fieldType publicInputs privateInputs -> interpret fieldType program publicInputs privateInputs >>= print
    Witness fieldType publicInputs privateInputs outputFilePath -> witness' outputFilePath fieldType program publicInputs privateInputs >>= print
    Prove fieldType publicInputs privateInputs circuitPath witnessPath proofPath ->
      prove' circuitPath witnessPath "aurora/parameter.json" proofPath fieldType program publicInputs privateInputs
    Verify circuitPath witnessPath proofPath ->
      verify' circuitPath witnessPath "aurora/parameter.json" proofPath
    Version -> putStrLn keelungVersion
  where
    printResult (Left err) = print err
    printResult (Right result) = print result

--------------------------------------------------------------------------------

-- | Compile a program to a 'R1CS' constraint system.
compile :: (Encode t) => FieldType -> Comp t -> IO (Either Error (R1CS Integer))
compile = compileWithOpts 1 [] []

-- | Compile a program to a 'R1CS' constraint system with optimization level 0.
compileO0 :: (Encode t) => FieldType -> Comp t -> IO (Either Error (R1CS Integer))
compileO0 = compileWithOpts 0 [] []

-- | Compile a program to a 'R1CS' constraint system with optimization level and RTS options as arguments.
compileWithOpts :: (Encode t) => Int -> [String] -> [String] -> FieldType -> Comp t -> IO (Either Error (R1CS Integer))
compileWithOpts level opts rtsopts fieldType prog = runM $ do
  elab <- liftEither (elaborateAndEncode prog)
  let options = "protocol" : optOptimize level : opts <> ["+RTS"] <> rtsopts <> ["-RTS"]
  callKeelungc options (fieldType, elab) :: M (R1CS Integer)
  where
    optOptimize :: Int -> String
    optOptimize i = "O" <> show i

-- | Default RTS options for profiling
rtsoptProf :: [String]
rtsoptProf = ["-p"]

-- | Helper function for compiling RTS options
-- Memory size in GB for RTS options -M, -H and in MB for -A
-- Try to increase if keelungc produces segmentation fault.
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/runtime_control.html
rtsoptMemory :: Int -> Int -> Int -> [String]
rtsoptMemory m h a = ["-M" <> show m <> "G", "-H" <> show h <> "G", "-A" <> show a <> "M"]

--------------------------------------------------------------------------------

stringToJsonInputs :: (FromJSON t, Inputable t) => (t -> t -> IO ()) -> BS.ByteString -> BS.ByteString -> IO ()
stringToJsonInputs (f :: t -> t -> IO ()) a b = let a' = decodeStrict a :: Maybe t
                                                    b' = decodeStrict b :: Maybe t
                                                in case (a', b') of
                                                 (Just i, Just j) -> f i j
                                                 (_, _) -> error "failed to decode JSON"

jsonFileInputs :: (FromJSON t, Inputable t) => (t -> t -> IO ()) -> FilePath -> FilePath -> IO ()
jsonFileInputs f a b = do a' <- readFile a
                          b' <- readFile b
                          stringToJsonInputs f (BS.pack a') (BS.pack b')

-- Default method of JSON inputs
jsonInputs :: (FromJSON t, Inputable t) => (t -> t -> IO ()) -> IO ()
jsonInputs f = jsonFileInputs f "public.json" "private.json"

-- | Generate a proof given circuit, inputs (witness), paratemer, and proof
prove' ::
  (Encode t) =>
  FilePath ->
  FilePath ->
  FilePath ->
  FilePath ->
  FieldType ->
  Comp t ->
  [Integer] ->
  [Integer] ->
  IO ()
prove' circuitPath witnessPath paramPath proofPath fieldType prog publicInput privateInput = do
  result <- runM $ do
    (cmd, args) <- findAuroraProver
    _ <- genCircuit circuitPath fieldType prog
    _ <- genWitness_ witnessPath fieldType prog publicInput privateInput -- Should generate public as well as private inputs
    -- _ <- genWitness_ witnessPath fieldType prog publicInput privateInput -- Should generate public as well as private inputs
    genParameters paramPath
    -- genInputs inputs publicInput -- Should generate public inputs only for verifier
    lift $ do
      let arguments =
            args
              ++ [ "--r1cs_filepath",
                   circuitPath,
                   "--input_filepath",
                   witnessPath,
                   "--parameter_filepath",
                   paramPath,
                   "--output_filepath",
                   proofPath
                 ]
      -- print $ "Running: " ++ cmd ++ " " ++ unwords arguments
      msg <- Process.readProcess cmd arguments mempty
      return (proofPath, msg)
  case result of
    Left err -> print err
    Right (_, msg) -> putStr msg

prove :: (Encode t) => FieldType -> Comp t -> [Integer] -> [Integer] -> IO ()
prove f p i o = do
  Path.createDirectoryIfMissing True "aurora"
  prove' "aurora/circuit.jsonl" "aurora/witness.jsonl" "aurora/parameter.json" "aurora/proof" f p i o

proveData :: (Encode t, Inputable a1, Inputable a2) => FieldType -> Comp t -> a1 -> a2 -> IO ()
proveData f p i o = prove f p (toInts i) (toInts o)

-- | Generate and verify a proof given circuit, witness, paratemer, and proof
verify' :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
verify' circuitPath witnessPath paramPath proofPath = do
  result <- runM $ do
    (cmd, args) <- findAuroraVerifier
    genParameters proofPath
    lift $ do
      let arguments =
            args
              ++ [ "--r1cs_filepath",
                   circuitPath,
                   "--input_filepath",
                   witnessPath,
                   "--parameter_filepath",
                   paramPath,
                   "--proof_filepath",
                   proofPath
                 ]
      Process.readProcess cmd arguments mempty
  case result of
    Left err -> print err
    Right msg -> putStr msg

-- TODO: Verify inputs.jsonl instead
verify :: IO ()
verify = verify' "aurora/circuit.jsonl" "aurora/witness.jsonl" "aurora/parameter.json" "aurora/proof"

-- | Compile a program as R1CS and write it to circuit.jsonl.
genCircuit :: (Encode t) => FilePath -> FieldType -> Comp t -> M (R1CS Integer)
genCircuit filePath fieldType prog = do
  elab <- liftEither (elaborateAndEncode prog)
  r1cs <- callKeelungc ["protocol", "toJSON", "--filepath", filePath] (fieldType, elab) :: M (R1CS Integer)
  liftIO $ putStrLn $ "Generated circuit file at: " <> filePath
  return r1cs

genCircuitBin :: (Encode t) => FilePath -> FieldType -> Comp t -> IO (Either Error String)
genCircuitBin filePath fieldType prog = runM $ do
  elab <- liftEither (elaborateAndEncode prog)
  _ <- callKeelungc ["protocol", "genCircuitBin", "--filepath", filePath] (fieldType, elab) :: M (R1CS Integer)
  liftIO $ (\s -> putStrLn $ "Generated binary circuit file at: " <> s) =<< Path.makeAbsolute filePath
  return "Success"

-- genCircuitDefault :: Encode t => FieldType -> Comp t -> M (R1CS Integer)
-- genCircuitDefault = genCircuit "aurora/circuit.jsonl"

-- | Generate witnesses for a program with inputs and write them to witness.jsonl.
genWitness_ :: (Encode t) => FilePath -> FieldType -> Comp t -> [Integer] -> [Integer] -> M [Integer]
genWitness_ filePath fieldType prog publicInput privateInput = do
  elab <- liftEither (elaborateAndEncode prog)
  output <- callKeelungc ["protocol", "genWitness", "--filepath", filePath] (fieldType, elab, publicInput, privateInput)
  liftIO $ putStrLn $ "Generated witness file at: " <> filePath
  return output

-- | Generate witnesses for a program with inputs and write them to witness.jsonl.
genWtns :: (Encode t) => FilePath -> FieldType -> Comp t -> [Integer] -> [Integer] -> IO (Either Error String)
genWtns filePath fieldType prog publicInput privateInput = runM $ do
  elab <- liftEither (elaborateAndEncode prog)
  _ <- callKeelungc ["protocol", "genWtns", "--filepath", filePath] (fieldType, elab, publicInput, privateInput) :: M [Integer]
  liftIO $ (\s -> putStrLn $ "Generated wtns file at: " <> s) =<< Path.makeAbsolute filePath
  return "Success"

-- | Generate parameters for a program and write them to parameter.json.
genParameters :: FilePath -> M ()
genParameters filePath = do
  lift $ BS.writeFile filePath "{\"security_level\": 128, \"heuristic_ldt_reducer_soundness\": true, \"heuristic_fri_soundness\": true, \"bcs_hash_type\": \"blake2b_type\", \"make_zk\": false, \"parallel\": true, \"field_size\": 181, \"is_multiplicative\": true}"
  liftIO $ putStrLn $ "Generated parameter file at: " <> filePath

-- | For generating witness.jsonl
witness' :: (Encode t) => FilePath -> FieldType -> Comp t -> [Integer] -> [Integer] -> IO [Integer]
witness' fp fieldType prog publicInput privateInput = runM (genWitness_ fp fieldType prog publicInput privateInput) >>= printErrorInstead

witness :: (Encode t) => FieldType -> Comp t -> [Integer] -> [Integer] -> IO [Integer]
witness = witness' "aurora/witness.jsonl"

-- | For generating inputs.jsonl
genInputs :: FilePath -> [Integer] -> M ()
genInputs fp inputs = do
  let inputs' = intercalate "," $ map ((\x -> "\"" ++ x ++ "\"") . show) inputs
  lift $ BS.writeFile fp $ fromString $ "{\"inputs\":[" ++ inputs' ++ "]}"

genInputsDefault :: [Integer] -> M ()
genInputsDefault = genInputs "inputs.jsonl"

--------------------------------------------------------------------------------

printErrorInstead :: (Show e) => Either e [a] -> IO [a]
printErrorInstead (Left err) = do
  print err
  return []
printErrorInstead (Right values) = return values

-- | Interpret a program with public and private inputs
interpret :: (Encode t) => FieldType -> Comp t -> [Integer] -> [Integer] -> IO [Integer]
interpret fieldType prog publicInput privateInput = interpretEither fieldType prog publicInput privateInput >>= printErrorInstead

-- | Interpret a program where public and private inputs are provided as datatype encodable to JSON.
interpretData :: (Encode t, Inputable a, Inputable b) => FieldType -> Comp t -> a -> b -> IO [Integer]
interpretData fieldType prog pub prv = interpret fieldType prog (toInts pub) (toInts prv)

-- | Interpret a program with public and private inputs
interpretEither :: (Encode t) => FieldType -> Comp t -> [Integer] -> [Integer] -> IO (Either Error [Integer])
interpretEither fieldType prog publicInput privateInput =
  runM
    ( do
        elab <- liftEither (elaborateAndEncode prog)
        callKeelungc ["protocol", "interpret"] (fieldType, elab, publicInput, privateInput)
    )
interpretDataEither :: (Encode t, Inputable a, Inputable b) => FieldType -> Comp t -> a -> b -> IO (Either Error [Integer])
interpretDataEither fieldType prog publicInput privateInput = interpretEither fieldType prog (toInts publicInput) (toInts privateInput)

--------------------------------------------------------------------------------

-- | Solves the R1CS of a Keelung program with given inputs and outputs the result
solveOutput :: (Encode t) => FieldType -> Comp t -> [Integer] -> [Integer] -> IO [Integer]
solveOutput fieldType prog publicInput privateInput = solveOutputEither fieldType prog publicInput privateInput >>= printErrorInstead

solveOutputData :: (Encode t, Inputable d, Inputable e) => FieldType -> Comp t -> d -> e -> IO [Integer]
solveOutputData fieldType prog pub prv = solveOutput fieldType prog (toInts pub) (toInts prv)

-- | Solves the R1CS of a Keelung program with given inputs and outputs the result
solveOutputEither :: (Encode t) => FieldType -> Comp t -> [Integer] -> [Integer] -> IO (Either Error [Integer])
solveOutputEither fieldType prog publicInput privateInput =
  runM
    ( do
        elab <- liftEither (elaborateAndEncode prog)
        callKeelungc ["protocol", "solve"] (fieldType, elab, publicInput, privateInput)
    )

solveOutputDataEither :: (Encode t, Inputable d, Inputable e) => FieldType -> Comp t -> d -> e -> IO (Either Error [Integer])
solveOutputDataEither fieldType prog pub prv = solveOutputEither fieldType prog (toInts pub) (toInts prv)

setupSnarkjsPtau :: Integer -> String -> IO (Either Error String)
setupSnarkjsPtau size entropy = runM $ do
  let p = show size
  lift $ Path.createDirectoryIfMissing True "snarkjs"
  fp <- lift $ Path.makeAbsolute "snarkjs"
  lift $ putStrLn "These generated files are for testings only and not production-ready.\nGenerating powersoftau files..."
  let pot :: String -> String -> String
      pot s i = fp <> "/pot" <> s <> "_" <> i <> ".ptau"
  _ <- putStrLn <$> callSnarkjs ["powersoftau", "new", "bn128", p, pot p "0"] 
  _ <- putStrLn <$> callSnarkjs ["powersoftau", "contribute", pot p "0", pot p "1", "-n=\"First Contribution\"", "-e=" <> entropy]
  _ <- putStrLn <$> callSnarkjs ["powersoftau", "beacon", pot p "1", pot p "beacon", "0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f", "10", "-n\"Beacon\"", "-e=" <> entropy]
  _ <- putStrLn <$> callSnarkjs ["powersoftau", "prepare", "phase2", pot p "beacon", pot p "final"]
  return $ "Generated .ptau file: " <> pot p "final"
  
checkWtnsBySnarkjs :: FilePath -> FilePath -> IO (Either Error String)
checkWtnsBySnarkjs r1cs wtns = runM $ callSnarkjs ["wtns", "check", r1cs, wtns] >> return "Success"

data SnarkjsBackend = Groth16 | PLONK

genZkeyBySnarkjs :: SnarkjsBackend -> FilePath -> FilePath -> IO (Either Error String)
genZkeyBySnarkjs b r1cs ptau = runM $ do
  let backend = case b of Groth16 -> "groth16";  PLONK -> "plonk";
  fp <- lift $ Path.makeAbsolute r1cs
  let parent = take (length fp - length r1cs) fp
      name = take (length r1cs - 5) r1cs
  _ <- callSnarkjs [backend, "setup", r1cs, ptau, parent <> name <> ".zkey"]
  return $ "Generated .zkey file: " <> parent <> name <> ".zkey"
                             
proveBySnarkjs :: SnarkjsBackend -> String -> String -> IO (Either Error String)
proveBySnarkjs b zkey wtns = runM $ do
  let backend = case b of Groth16 -> "groth16";  PLONK -> "plonk";
  callSnarkjs [backend, "prove", zkey, wtns]

--------------------------------------------------------------------------------

-- | Internal function for handling data serialization
callKeelungc :: (Serialize a, Serialize b) => [String] -> a -> M b
callKeelungc args' payload = do
  (cmd, args) <- findKeelungc
  version <- readKeelungVersion cmd args
  checkCompilerVersion version
  blob <- lift $ Process.readProcess cmd (args ++ args') (BS.unpack $ Serialize.encode payload)
  let result = Serialize.decode (BS.pack blob)
  case result of
    Left err -> throwError (DecodeError err)
    Right (Left err) -> throwError (CompileError err)
    Right (Right x) -> return x

callSnarkjs :: [String] -> M String
callSnarkjs args' = do
  (cmd, args) <- findSnarkjs
  -- TODO: check for Snarkjs' version
  (code, out, err) <- lift $ Process.readProcessWithExitCode cmd (args ++ args') [] 
  if code == ExitSuccess || code == ExitFailure 99 then
    return out
  else
    throwError (SnarkjsError (err <> "\n" <> out))

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
        then do
          lift $ Path.createDirectoryIfMissing True "aurora"
          filepath <- lift $ Path.makeAbsolute "aurora"
          case System.Info.arch of
            "x86_64" -> return ("docker", ["run", "-i", "--volume", filepath ++ ":/aurora", "btqag/keelungc"])
            -- insert "--platform=linux/amd64" when we are not on a x86 machine
            _ -> return ("docker", ["run", "-i", "--platform=linux/amd64", "--volume", filepath ++ ":/aurora", "btqag/keelungc"])
        else throwError CannotLocateKeelungC

findAuroraProver :: M (String, [String])
findAuroraProver = do
  auroraExists <- checkCmd "aurora_prove"
  if auroraExists
    then return ("aurora_prove", [])
    else do
      dockerExists <- checkCmd "docker"
      -- let currentDirector = Path.getCurrentDirectory
      if dockerExists
        then do
          lift $ Path.createDirectoryIfMissing True "aurora"
          filepath <- lift $ Path.makeAbsolute "aurora"
          case System.Info.arch of
            "x86_64" -> return ("docker", ["run", "-i", "--volume", filepath ++ ":/aurora", "btqag/aurora-prove"])
            -- insert "--platform=linux/amd64" when we are not on a x86 machine
            _ -> return ("docker", ["run", "-i", "--platform=linux/amd64", "--volume", filepath ++ ":/aurora", "btqag/aurora-prove"])
        else throwError CannotLocateProver

findAuroraVerifier :: M (String, [String])
findAuroraVerifier = do
  auroraExists <- checkCmd "aurora_verify"
  if auroraExists
    then return ("aurora_verify", [])
    else do
      dockerExists <- checkCmd "docker"
      if dockerExists
        then do
          lift $ Path.createDirectoryIfMissing True "aurora"
          filepath <- lift $ Path.makeAbsolute "aurora"
          case System.Info.arch of
            "x86_64" -> return ("docker", ["run", "-i", "--volume", filepath ++ ":/aurora", "btqag/aurora-verify"])
            -- insert "--platform=linux/amd64" when we are not on a x86 machine
            _ -> return ("docker", ["run", "-i", "--platform=linux/amd64", "--volume", filepath ++ ":/aurora", "btqag/aurora-verify"])
        else throwError CannotLocateVerifier
  

findSnarkjs :: M (String, [String])
findSnarkjs = do
  snarkJsExists <- checkCmd "snarkjs"
  if snarkJsExists then
    return ("snarkjs", [])
  else
    throwError CannotLocateSnarkjs

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

-- | Check if the compiler version matches the version of this library
--   patch number can be different
checkCompilerVersion :: (Int, Int, Int) -> M ()
checkCompilerVersion (major, minor, patch) = do
  let (compilerMajor, compilerMinor, compilerPatch) = keelungCompilerVersion
  if (major, minor) == (compilerMajor, compilerMinor) && patch >= compilerPatch
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

type M = ExceptT Error IO

runM :: M a -> IO (Either Error a)
runM = runExceptT

-- | Handle 'IO' Exceptions in the 'M' Monad
catchIOError :: Error -> IO a -> M a
catchIOError err f = lift (IO.catchIOError (Right <$> f) (const (return (Left err)))) >>= liftEither

--------------------------------------------------------------------------------

instance (Encode a) => Show (Comp a) where
  show prog = case elaborateAndEncode prog of
    Left err -> show err
    Right elaborated -> show elaborated
