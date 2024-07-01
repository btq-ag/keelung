{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Keelung.SnarkjsBinary where

import Data.ByteString ()
import Data.ByteString.Char8 ()
import Data.Attoparsec.ByteString as P
    ( string, take, count, Parser, satisfyWith )
import Debug.Trace (trace)
import Data.Attoparsec.ByteString.Char8 as C
import Data.Char (digitToInt)
import Data.Int
import Data.ByteString as BS
import Control.Monad.State
import Control.Applicative

data R1CSHeader = R1CSHeader {
    fieldSize :: Int,
    prime :: Integer,
    wires :: Int,
    nPubOut :: Int,
    nPubIn :: Int,
    nPrvIn :: Int,
    nLabels :: Int64,
    mConstraints :: Int
}

data R1CSBin = R1CSBin {
    version :: Int,
    sectionNumber :: Int,
    header :: R1CSHeader
}

parseR1cs :: StateT (Bool, Bool) Parser R1CSBin
parseR1cs = do
    _ <- lift (string "r1cs" <?> "File is not R1CS.")
    version <- lift $ toIntegerLE <$> P.take 4
    nSecs <- lift $ toIntegerLE <$> P.take 4
    secs <- manyTill (do
        sectionType <- lift takeInt
        sectionSize <- lift takeInt
        return _
        )
        (lift endOfInput)
    return _
  where
    parseHeader :: Parser R1CSHeader
    parseHeader = do
        fieldSize <- takeInt 4
        primeSize <- toIntegerLE <$> P.take fieldSize
        nWires <- takeInt 4
        nPubIn <- takeInt 4
        nPubOut <- takeInt 4
        nPrvIn <- takeInt 4
        nLabels <- takeInt 8
        mConstraints <- takeInt
        return $ R1CSHeader fieldSize primeSize nWires nPubIn nPubOut nPrvIn (fromIntegral nLabels) mConstraints
    parseConstraints :: Int -> Parser R1CSBin
    parseConstraints = _

toIntegerLE :: ByteString -> Integer
toIntegerLE = BS.foldr (\ w i -> toInteger w + i * 256) 0

takeInt :: Int -> Parser Int
takeInt i = fromIntegral . toIntegerLE <$> P.take i

parseWtns :: Parser ()
parseWtns = do
    _ <- string "wtns"
    return _