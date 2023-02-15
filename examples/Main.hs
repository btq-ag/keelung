{-# LANGUAGE DataKinds #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Redundant return" #-}
{-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

-- import Control.Monad (forM_)

import Control.DeepSeq (NFData (rnf))
import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.Bits (Bits (testBit))
import Data.Word (Word8)
import Keelung

-- import Control.Monad

-- | Outputs the given field element
echo :: Comp Field
echo = do
  x <- input Public -- request for an input and bind it to 'x'
  return x -- return 'x'

-- | A program that expects 2 inputs and returns no output
useless :: Comp ()
useless = do
  _x <- inputField Public -- request for an input and bind it to 'x'
  _y <- inputBool Public -- request for an input and bind it to 'y'
  return () -- return nothing

-- Formula: (0°C × 9/5) + 32 = 32°F
tempConvert :: Comp Field
tempConvert = do
  toFahrenheit <- input Public
  degree <- input Public
  return $
    cond
      toFahrenheit
      (degree * 9 / 5 + 32)
      (degree - 32 * 5 / 9)

terminationProblem :: Comp [[Boolean]]
terminationProblem = return $ map fromChar "A"
  where
    -- Construct a W8 from a Word8
    fromWord8 :: Word8 -> [Boolean]
    fromWord8 word = Prelude.map (Boolean . testBit word) [0 .. 7]

    -- Construct a W8 from a Char
    fromChar :: Char -> [Boolean]
    fromChar = fromWord8 . toEnum . fromEnum

main :: IO ()
main = evaluate $ rnf $ elaborateAndEncode (return $ fromString' (string 400000))
  where
    -- \| `fromWord8` implemented with immutable arrays
    fromWord8' :: Word8 -> [Boolean]
    fromWord8' word = Prelude.map (Boolean . testBit word) [0 .. 7]

    -- \| `fromChar` implemented with immutable arrays
    fromChar' :: Char -> [Boolean]
    fromChar' = fromWord8' . toEnum . fromEnum

    -- \| `fromString` implemented with immutable arrays
    fromString' :: String -> [[Boolean]]
    fromString' = map fromChar'

    string :: Int -> String
    string n = concat $ replicate n "Hello world"

assertToBe42 :: Comp ()
assertToBe42 = do
  x <- inputField Public
  assert (x `eq` 42)

-- | A program that expects the second input to be the square of the first input
-- This program returns no output
assertSquare :: Comp ()
assertSquare = do
  x <- inputField Public
  y <- inputField Public
  assert ((x * x) `eq` y)

loop3 :: Int -> Int -> Comp ()
loop3 n m = do
  xs <- inputList2 Public n m
  -- expecting square of signatures as input
  squares <- inputList2 Public n m
  -- for each signature
  forM_ [0 .. n - 1] $ \i -> do
    -- for each term of signature
    forM_ [0 .. m - 1] $ \j -> do
      let x = xs !! i !! j
      let x' = squares !! i !! j
      assert ((x' :: Field) `eq` (x * x))

uint :: Comp [Boolean]
uint = do
  x <- inputUInt @4 Public
  return [x !!! 0, x !!! 1, x !!! 2, x !!! 3]
