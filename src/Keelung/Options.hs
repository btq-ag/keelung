module Keelung.Options (Options (..), getOptions) where

import Keelung.Field
import Options.Applicative

data Options
  = Compile FieldType
  | Interpret FieldType [Integer] [Integer]
  | Witness FieldType [Integer] [Integer] FilePath
  | Prove
      FieldType
      [Integer]
      [Integer]
      FilePath -- Circuit input filepath
      FilePath -- Witness input filepath
      FilePath -- Parameter input filepath
      FilePath -- Proof output filepath
  | Version
  deriving (Show)

-- | Command line option parser
getOptions :: IO Options
getOptions =
  execParser $
    info
      (options <**> helper)
      ( fullDesc
          <> header "Keelung - DSL for constructing ZKPs"
      )

options :: Parser Options
options =
  hsubparser
    ( metavar "COMMAND"
        <> commandGroup "Commands:"
        <> command
          "compile"
          ( info
              (Compile <$> parseFieldType)
              (fullDesc <> progDesc "Compile Keelung programs")
          )
        <> command
          "interpret"
          ( info
              (Interpret <$> parseFieldType <*> parseInputs "List of public inputs" <*> parseInputs "List of private inputs")
              (fullDesc <> progDesc "Interpret Keelung programs")
          )
        <> command
          "witness"
          ( info
              ( Witness
                  <$> parseFieldType
                  <*> parseInputs "List of public inputs"
                  <*> parseInputs "List of private inputs"
                  <*> parseFilePath "output" 'o' "aurora/witness.jsonl"
              )
              (fullDesc <> progDesc "Generate witness of Keelung programs with inputs")
          )
        <> command
          "prove"
          ( info
              ( Prove
                  <$> parseFieldType
                  <*> parseInputs "List of public inputs"
                  <*> parseInputs "List of private inputs"
                  <*> parseFilePath "circuit" 'c' "aurora/circuit.jsonl"
                  <*> parseFilePath "witness" 'w' "aurora/witness.jsonl"
                  <*> parseFilePath "param" 'p' "aurora/parameter.json"
                  <*> parseFilePath "output" 'o' "aurora/proof"
              )
              (fullDesc <> progDesc "Generate proof of Keelung programs with inputs and witnesses")
          )
    )
    <|> flag' Version (long "version" <> short 'v' <> help "Show version")

parseInputs :: String -> Parser [Integer]
parseInputs msg = argument auto (help msg <> metavar "[Integer]")

parseFilePath :: String -> Char -> FilePath -> Parser FilePath
parseFilePath longName shortName defaultPath =
  strOption
    ( long longName
        <> short shortName
        <> showDefault
        <> value defaultPath
        <> metavar "FilePath"
    )

parseFieldType :: Parser FieldType
parseFieldType =
  hsubparser
    ( metavar "FIELD-TYPE"
        <> commandGroup "Named field types:"
        <> command
          "gf181"
          ( info
              (pure gf181 <**> helper)
              ( fullDesc
                  <> progDesc "Prime 1552511030102430251236801561344621993261920897571225601"
              )
          )
        <> command
          "b64"
          ( info
              (pure bn128 <**> helper)
              ( fullDesc
                  <> progDesc "Binary 18446744073709551643"
              )
          )
        <> command
          "bn128"
          ( info
              (pure bn128 <**> helper)
              ( fullDesc
                  <> progDesc "Prime 21888242871839275222246405745257275088548364400416034343698204186575808495617"
              )
          )
    )
    <|> ( Prime
            <$> option
              auto
              ( short 'p'
                  <> long "prime"
                  <> help "Order of the prime field"
                  <> metavar "Integer"
              )
        )
    <|> ( Binary
            <$> option
              auto
              ( short 'b'
                  <> long "binary"
                  <> help "Order of the binary field"
                  <> metavar "Integer"
              )
        )