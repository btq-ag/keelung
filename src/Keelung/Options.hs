module Keelung.Options (Options (..), getOptions) where

import Keelung.Field
import Options.Applicative

data Options
  = Compile FieldType
  | Interpret FieldType [Integer] [Integer]
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
              (Interpret <$> parseFieldType <*> parseInputs "List of public inputs" <*> parseInputs "List of pricate inputs")
              (fullDesc <> progDesc "Interpret Keelung programs")
          )
    )
    <|> flag' Version (long "version" <> short 'v' <> help "Show version")

parseInputs :: String -> Parser [Integer]
parseInputs msg = argument auto (help msg <> metavar "[Integer]")

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