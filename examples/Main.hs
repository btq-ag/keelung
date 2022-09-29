{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Redundant return" #-}

module Main where

-- import Control.Monad (forM_)

import Control.Monad (forM_)
import Data.Bits (Bits (testBit))
import Data.Word (Word8)
import Keelung
import Control.DeepSeq (NFData(rnf))
import Control.Exception (evaluate)

-- import Control.Monad

-- | Outputs whether number is given.
echo :: Comp Number
echo = do
  x <- input -- request for an input and bind it to 'x'
  return x -- return 'x'

-- | A program that expects 2 inputs and returns no output
useless :: Comp ()
useless = do
  _x <- inputNum -- request for an input and bind it to 'x'
  _y <- inputBool -- request for an input and bind it to 'y'
  return () -- return nothing

-- Formula: (0°C × 9/5) + 32 = 32°F
tempConvert :: Comp Number
tempConvert = do
  toFahrenheit <- input
  degree <- input
  return $
    cond
      toFahrenheit
      (degree * 9 / 5 + 32)
      (degree - 32 * 5 / 9)

terminationProblem :: Comp (Arr (Arr Boolean))
terminationProblem = return $ go "A"
  where
    -- Construct a W8 from a Word8
    fromWord8 :: Word8 -> Arr Boolean
    fromWord8 word = toArray $ Prelude.map (Boolean . testBit word) [0 .. 7]

    -- Construct a W8 from a Char
    fromChar :: Char -> Arr Boolean
    fromChar = fromWord8 . toEnum . fromEnum

    -- Construct an array of W8s from a String
    go :: String -> Arr (Arr Boolean)
    go xs = toArray (map fromChar xs)

-- |
main :: IO ()
main = evaluate $ rnf $ elaborate (return $ fromString' (string 400000))
  where

    -- | `fromWord8` implemented with immutable arrays
    fromWord8' :: Word8 -> Arr Boolean
    fromWord8' word = toArray $ Prelude.map (Boolean . testBit word) [0 .. 7]

    -- | `fromChar` implemented with immutable arrays
    fromChar' :: Char -> Arr Boolean
    fromChar' = fromWord8' . toEnum . fromEnum

    -- | `fromString` implemented with immutable arrays
    fromString' :: String -> Arr (Arr Boolean)
    fromString' = toArray . map fromChar'

    string :: Int -> String
    string n = concat $ replicate n "Hello world"

assertToBe42 :: Comp ()
assertToBe42 = do
  x <- input
  assert (x `Eq` 42)

-- | A program that expects the second input to be the square of the first input
-- This program returns no output
assertSquare :: Comp ()
assertSquare = do
  x <- input
  y <- input
  assert ((x * x) `Eq` y)

loop3 :: Int -> Int -> Comp ()
loop3 n m = do
  xs <- inputs2 n m
  -- expecting square of signatures as input
  squares <- inputs2 n m
  -- for each signature
  forM_ [0 .. n - 1] $ \i -> do
    -- for each term of signature
    forM_ [0 .. m - 1] $ \j -> do
      let x = access2 xs (i, j)
      let x' = access2 squares (i, j)
      assert (x' `Eq` (x * x))
