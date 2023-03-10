{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Keelung is a DSL for building zero-knowledge proofs
module Keelung
  ( module Keelung.Syntax,
    module Keelung.Field,
    module Keelung.Heap,
    module Keelung.Monad,
    module Keelung.Data.Bits,
    run,
    compile,
    compileO0,
    compileO2,
    compileWithOpts,
    rtsoptProf,
    rtsoptMemory,
    generate,
    generateDefault,
    verify,
    verifyDefault,
    genCircuit,
    genCircuitDefault,
    genWitness,
    genWitnessDefault,
    genInputs,
    genInputsDefault,
    interpret_,
    interpret,
    gf181,
    bn128,
    b64,
    elaborateAndEncode,
    Encode,
    GaloisField,
    keelungVersion,
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
import Keelung.Data.Struct (Struct (..))
import Keelung.Error
import Keelung.Field
import Keelung.Heap
import Keelung.Monad
import Keelung.Syntax
import Keelung.Syntax.Encode
import Keelung.Syntax.Encode.Syntax qualified as Encoding
import System.Directory qualified as Path
import System.IO.Error qualified as IO
import System.Info qualified
import System.Process qualified as Process
import Text.Read (readMaybe)

--------------------------------------------------------------------------------

-- | Compile a program to a 'R1CS' constraint system.
compile :: Encode t => FieldType -> Comp t -> IO (Either Error (R1CS Integer))
compile = compileWithOpts 1 [] []

-- | Compile a program to a 'R1CS' constraint system with optimization level 0.
compileO0 :: Encode t => FieldType -> Comp t -> IO (Either Error (R1CS Integer))
compileO0 = compileWithOpts 0 [] []

-- | Compile a program to a 'R1CS' constraint system with optimization level 2.
compileO2 :: Encode t => FieldType -> Comp t -> IO (Either Error (R1CS Integer))
compileO2 = compileWithOpts 2 [] []

-- | Compile a program to a 'R1CS' constraint system with optimization level and RTS options as arguments.
compileWithOpts :: Encode t => Int -> [String] -> [String] -> FieldType -> Comp t -> IO (Either Error (R1CS Integer))
compileWithOpts level opts rtsopts fieldType prog = runM $ do
  elab <- liftEither (elaborateAndEncode prog)
  let opts' = "protocol" : optOptimize level : opts <> ["+RTS"] <> rtsopts <> ["-RTS"]
  case fieldType of
    GF181 -> convertFieldElement (wrapper opts' (fieldType, elab) :: M (R1CS GF181))
    BN128 -> convertFieldElement (wrapper opts' (fieldType, elab) :: M (R1CS BN128))
    B64 -> convertFieldElement (wrapper opts' (fieldType, elab) :: M (R1CS B64))
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

-- | Generate a proof given circuit, inputs (witness), paratemer, and proof
generate_ :: (Serialize n, Integral n, Encode t) =>
  FilePath -> FilePath -> FilePath -> FilePath -> FilePath ->
  FieldType -> Comp t -> [n] -> [n] -> IO (Either Error (FilePath, String))
generate_ circuit witness _inputs param proofPath fieldType prog publicInput privateInput = runM $ do
  (cmd, args) <- findAuroraProver
  _ <- genCircuit circuit fieldType prog
  _ <- genWitness_ witness fieldType prog publicInput privateInput -- Should generate public as well as private inputs
  genParameters param
  -- genInputs inputs publicInput -- Should generate public inputs only for verifier
  lift $ do
    let arguments =
          args
            ++ [ "--r1cs_filepath",
                 circuit,
                 "--input_filepath",
                 witness,
                 "--parameter_filepath",
                 param,
                 "--output_filepath",
                 proofPath
               ]
    msg <- Process.readProcess cmd arguments mempty
    return (proofPath, msg)

-- | Generate a proof
generate :: Encode t =>
  FilePath -> FilePath -> FilePath -> FilePath -> FilePath ->
  FieldType -> Comp t -> [Integer] -> [Integer] -> IO ()
generate circuit witness inputs param proof fieldType prog publicInput privateInput = do
  result <- generate_ circuit witness inputs param proof fieldType prog publicInput privateInput
  case result of
    Left err -> print err
    Right (_, msg) -> putStr msg

generateDefault :: Encode t => FieldType -> Comp t -> [Integer] -> [Integer] -> IO ()
generateDefault fieldType prog publicInput privateInput = do
  Path.createDirectoryIfMissing True "aurora"
  generate "aurora/circuit.jsonl" "aurora/witness.jsonl" "aurora/inputs.jsonl" "aurora/parameter.json" "aurora/proof" fieldType prog publicInput privateInput

-- | Generate and verify a proof given circuit, inputs (witness), paratemer, and proof
verify_ :: FilePath -> FilePath -> FilePath -> FilePath -> IO (Either Error String)
verify_ circuit inputs param proof = runM $ do
  (cmd, args) <- findAuroraVerifier
  genParameters param
  lift $ do
    let arguments =
          args
            ++ [ "--r1cs_filepath",
                 circuit,
                 "--input_filepath",
                 inputs,
                 "--parameter_filepath",
                 param,
                 "--proof_filepath",
                 proof
               ]
    Process.readProcess cmd arguments mempty

-- | Verify a proof
verify :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
verify circuit inputs param proof = do
  result <- verify_ circuit inputs param proof 
  case result of
    Left err -> print err
    Right msg -> putStr msg

-- TODO: Verify inputs.jsonl instead
verifyDefault :: IO ()
verifyDefault = verify "aurora/circuit.jsonl" "aurora/witness.jsonl" "aurora/parameter.json" "aurora/proof"

-- | Compile a program as R1CS and write it to circuit.jsonl.
genCircuit :: Encode t => FilePath -> FieldType -> Comp t -> M (R1CS Integer)
genCircuit fp fieldType prog = do
  elab <- liftEither (elaborateAndEncode prog)
  case fieldType of
    GF181 -> convertFieldElement (wrapper ["protocol", "toJSON", "--filepath", fp] (fieldType, elab) :: M (R1CS GF181))
    BN128 -> convertFieldElement (wrapper ["protocol", "toJSON", "--filepath", fp] (fieldType, elab) :: M (R1CS BN128))
    B64 -> convertFieldElement (wrapper ["protocol", "toJSON", "--filepath", fp] (fieldType, elab) :: M (R1CS B64))

genCircuitDefault :: Encode t => FieldType -> Comp t -> M (R1CS Integer)
genCircuitDefault = genCircuit "aurora/circuit.jsonl"

-- | Generate witnesses for a program with inputs and write them to witness.jsonl.
genWitness_ :: (Serialize n, Integral n, Encode t) => FilePath -> FieldType -> Comp t -> [n] -> [n] -> M [n]
genWitness_ fp fieldType prog publicInput privateInput = do
  elab <- liftEither (elaborateAndEncode prog)
  wrapper ["protocol", "genWitness", "--filepath", fp] (fieldType, elab, map toInteger publicInput, map toInteger privateInput)

-- | Generate parameters for a program and write them to parameter.json.
genParameters :: FilePath -> M ()
genParameters fp = lift $ BS.writeFile fp "{\"security_level\": 128, \"heuristic_ldt_reducer_soundness\": true, \"heuristic_fri_soundness\": true, \"bcs_hash_type\": \"blake2b_type\", \"make_zk\": false, \"parallel\": true, \"field_size\": 181, \"is_multiplicative\": true}"

-- | For generating witness.jsonl
genWitness :: Encode t => FilePath -> FieldType -> Comp t -> [Integer] -> [Integer] -> IO [Integer]
genWitness fp fieldType prog publicInput privateInput = runM (genWitness_ fp fieldType prog publicInput privateInput) >>= printErrorInstead

genWitnessDefault :: Encode t => FieldType -> Comp t -> [Integer] -> [Integer] -> IO [Integer]
genWitnessDefault = genWitness "aurora/witness.jsonl"

-- | For generating inputs.jsonl
genInputs :: (Integral n) => FilePath -> [n] -> M ()
genInputs fp inputs = do
  let inputs' = intercalate "," $ map ((\x -> "\"" ++ x ++ "\"") . show . toInteger) inputs
  lift $ BS.writeFile fp $ fromString $ "{\"inputs\":[" ++ inputs' ++ "]}"

genInputsDefault :: (Integral n) => [n] -> M ()
genInputsDefault = genInputs "inputs.jsonl"

--------------------------------------------------------------------------------

-- | Interpret a program
interpret_ :: (Serialize n, Integral n, Encode t) => FieldType -> Comp t -> [n] -> [n] -> IO (Either Error [n])
interpret_ fieldType prog publicInput privateInput = runM $ do
  elab <- liftEither (elaborateAndEncode prog)
  wrapper ["protocol", "interpret"] (fieldType, elab, map toInteger publicInput, map toInteger privateInput)

printErrorInstead :: Show e => Either e [a] -> IO [a]
printErrorInstead (Left err) = do
  print err
  return []
printErrorInstead (Right values) = return values

-- | Interpret a program with public and private inputs
run :: Encode t => Comp t -> [Integer] -> [Integer] -> IO [Integer]
run prog publicInput privateInput = interpret_ GF181 prog publicInput privateInput >>= printErrorInstead

-- | Interpret a program with public and private inputs
interpret :: Encode t => FieldType -> Comp t -> [Integer] -> [Integer] -> IO [Integer]
interpret fieldType prog publicInput privateInput = interpret_ fieldType prog publicInput privateInput >>= printErrorInstead

-- | A specialized version of 'interpret' that outputs numbers as 'N GF181'
gf181 :: Encode t => Comp t -> [GF181] -> [GF181] -> IO [N GF181]
gf181 prog publicInput privateInput = map N <$> (interpret_ GF181 prog publicInput privateInput >>= printErrorInstead)

-- | A specialized version of 'interpret' that outputs numbers as 'N B64'
b64 :: Encode t => Comp t -> [B64] -> [B64] -> IO [N B64]
b64 prog publicInput privateInput = map N <$> (interpret_ B64 prog publicInput privateInput >>= printErrorInstead)

-- | A specialized version of 'interpret' that outputs numbers as 'N BN128'
bn128 :: Encode t => Comp t -> [BN128] -> [BN128] -> IO [N BN128]
bn128 prog publicInput privateInput = map N <$> (interpret_ BN128 prog publicInput privateInput >>= printErrorInstead)

--------------------------------------------------------------------------------

-- | Elaborate a program and encode it
elaborateAndEncode :: Encode t => Comp t -> Either Error Encoding.Elaborated
elaborateAndEncode prog = encodeElaborated <$> elaborate prog
  where
    encodeElaborated :: Encode t => Elaborated t -> Encoding.Elaborated
    encodeElaborated (Elaborated expr comp) = runHeapM (compHeap comp) $ do
      let Computation counters _addrSize _heap eb assertions divModRelsU = comp
       in Encoding.Elaborated
            <$> encode expr
            <*> ( Encoding.Computation
                    counters
                    <$> (Struct <$> mapM encode' (structF eb) <*> mapM encode' (structB eb) <*> pure (structU eb))
                    <*> mapM encode assertions
                    <*> pure divModRelsU
                )

--------------------------------------------------------------------------------

-- | Internal function for handling data serialization
wrapper :: (Serialize a, Serialize b) => [String] -> a -> M b
wrapper args' payload = do
  (cmd, args) <- findKeelungc
  version <- readKeelungVersion cmd args
  checkCompilerVersion version
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
        then do
          lift $ Path.createDirectoryIfMissing True "aurora"
          filepath <- lift $ Path.makeAbsolute "aurora"
          case System.Info.arch of
            "x86_64" -> return ("docker", ["run", "-i", "btqag/keelungc"])
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
            "x86_64" -> return ("docker", ["run", "-i", "btqag/aurora-prove"])
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
            "x86_64" -> return ("docker", ["run", "-i", "btqag/aurora-verify"])
            -- insert "--platform=linux/amd64" when we are not on a x86 machine
            _ -> return ("docker", ["run", "-i", "--platform=linux/amd64", "--volume", filepath ++ ":/aurora", "btqag/aurora-verify"])
        else throwError CannotLocateVerifier

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

checkCompilerVersion :: (Int, Int, Int) -> M ()
checkCompilerVersion (major, minor, patch) = do
  if major == 0 && minor >= 9 && minor < 10 && patch >= 3
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

-- | String of Keelung version exposed to the user
keelungVersion :: String
keelungVersion = let (major, minor, patch) = keelungVersion_ in show major ++ "." ++ show minor ++ "." ++ show patch
  where
    -- \| The version of Keelung is a triple of three numbers, we're not going full semver yet
    keelungVersion_ :: (Int, Int, Int)
    keelungVersion_ = (0, 9, 3)

--------------------------------------------------------------------------------

type M = ExceptT Error IO

runM :: M a -> IO (Either Error a)
runM = runExceptT

-- liftEitherT :: IO (Either Error a) -> M a
-- liftEitherT f = do
--   result <- lift f
--   case result of
--     Left err -> throwError err
--     Right x -> return x

-- | Handle 'IO' Exceptions in the 'M' Monad
catchIOError :: Error -> IO a -> M a
catchIOError err f = lift (IO.catchIOError (Right <$> f) (const (return (Left err)))) >>= liftEither

-- | Prettify and convert all field elements to 'Integer' in a 'R1CS'
convertFieldElement :: (GaloisField a, Integral a) => M (R1CS a) -> M (R1CS Integer)
convertFieldElement = fmap (fmap (toInteger . N))