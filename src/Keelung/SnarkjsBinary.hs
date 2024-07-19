{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE BlockArguments #-}
module Keelung.SnarkjsBinary where

import Data.ByteString ()
import Data.ByteString.Char8 ()
import Data.Attoparsec.ByteString as P
import Data.Int
import Data.IntMap qualified as IntMap
import Data.IntMap (IntMap)
import Data.ByteString qualified as BS ( foldr, readFile, ByteString, foldl, unpack )
import Control.Monad.State
import Data.List (sort)
import Debug.Trace (trace)
import Data.Maybe (fromMaybe)

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

data Constraint = Constraint {
    factorsA :: [(Int, Integer)],
    factorsB :: [(Int, Integer)],
    factorsC :: [(Int, Integer)]
}

instance Show Constraint where
    show (Constraint fAs fBs fCs) =
        let showPoly fs = if null fs
            then "0 (all coeffs are 0s)"
            else foldMap (\(k, v) -> (if (k, v) == head fs then "" else " + ") <> show v ++ "*w_" ++ show k) fs
                 -- let (lastK, _) = fromMaybe (error "impossible") $ IntMap.lookupLE (maxBound :: Int) fs
                 -- in  IntMap.foldMapWithKey (\k v -> show v ++ "*w_" ++ show k ++ if k == lastK then "" else " + ") fs
        in "  " ++ showPoly fAs ++ "\n * " ++ showPoly fBs ++ "\n = " ++ showPoly fCs

newtype R1csSection = R1csSection { unR1csSection :: (Int, BS.ByteString) }
  deriving Eq

instance Ord R1csSection where
    compare (R1csSection (i1, _)) (R1csSection (i2, _)) = compare i1 i2

readAndParseR1CS :: FilePath -> IO ()
readAndParseR1CS fp = do
    bs <- BS.readFile fp
    putStrLn "read success"
    let constraints = either error id do
          secs <- trace "parsing r1cs.." $ parseOnly parseR1csRaw bs
          let sortedSecs = trace "sorting.." $ sort secs
          when (length sortedSecs < 2) $ Left "Not enough sections!"
          let fstSec = snd $ unR1csSection (head sortedSecs)
              sndSec = snd $ unR1csSection (sortedSecs !! 1)
          trace ("sndSec: " ++ show (BS.unpack sndSec)) (return ())
          header <- trace "parsing header.." parseOnly parseHeader fstSec
          trace ("mConstraints: " ++ show (mConstraints header)) (return ())
          trace "parsing constraints.." $ parseOnly (count (mConstraints header) (parseConstraint $ fieldSize header)) sndSec
    forM_ (zip [1..length constraints] constraints) $ \(i, c) ->
        trace ("Constraint #" ++ show i ++ " :\n" ++ show c) (return ())
        --putStrLn $ "Constraint #" ++ show i ++ " :\n" ++ show c

parseR1csRaw :: Parser [R1csSection]
parseR1csRaw = do
    _ <- string "r1cs" <?> "File is not R1CS."
    _version <- takeInt 4
    nSecs   <- takeInt 4
    count nSecs $ do
        sectionType <- takeInt 4
        sectionSize <- takeInt 8
        trace ("section type: " ++ show sectionType) (pure ())
        trace ("section size: " ++ show sectionSize) (pure ())
        sec <- P.take sectionSize
        return $ R1csSection (sectionType, sec)

parseHeader :: Parser R1CSHeader
parseHeader = do
    fieldSize <- takeInt 4
    primeSize <- toIntegerLE <$> P.take fieldSize
    trace ("prime field: " ++ show primeSize) (return ())
    nWires <- takeInt 4
    nPubIn <- takeInt 4
    nPubOut <- takeInt 4
    nPrvIn <- takeInt 4
    trace ("nWires: " ++ show nWires ++  ", nPubIn: " ++ show nPubIn ++ ", nPubOut: " ++ show nPubOut ++ ", nPrvIn: " ++ show nPrvIn) (return ())
    nLabels <- takeInt 8
    mConstraints <- takeInt 4
    return $ R1CSHeader fieldSize primeSize nWires nPubIn nPubOut nPrvIn (fromIntegral nLabels) mConstraints

parseConstraint :: Int -> Parser Constraint
parseConstraint fieldSize = do
    aMap <- parsePoly fieldSize
    bMap <- parsePoly fieldSize
    cMap <- parsePoly fieldSize
    return $ Constraint aMap bMap cMap


parsePoly :: Int -> Parser [(Int, Integer)]
parsePoly fieldSize = do
    n <- takeInt 4
    count n $ do
        wireId <- takeInt 4
        coeff <- toIntegerLE <$> P.take fieldSize
        trace ("wireId: " ++ show wireId ++ ", coefficient: " ++ show coeff) (return ())
        return (wireId, coeff)
    -- return $ foldl (\m (k, v) -> IntMap.insert k v m) IntMap.empty factors

toIntegerLE :: BS.ByteString -> Integer
toIntegerLE = BS.foldr (\ w i -> toInteger w + i * 256) 0

takeInt :: Int -> Parser Int
takeInt i = fromIntegral . toIntegerLE <$> P.take i

-- parseWtns :: Parser ()
-- parseWtns = do
--     _ <- string "wtns"
--     return _