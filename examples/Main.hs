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

-- import Control.Monad

-- | Outputs whether number is given.
echo :: Comp GF181 (Val 'Num GF181)
echo = do
  x <- input -- request for an input and bind it to 'x'
  return x -- return 'x'

-- | A program that expects 2 inputs and returns no output
useless :: Comp GF181 (Val 'Unit GF181)
useless = do
  _x <- inputNum -- request for an input and bind it to 'x'
  _y <- inputBool -- request for an input and bind it to 'y'
  return unit -- return nothing

-- Formula: (0°C × 9/5) + 32 = 32°F
tempConvert :: Comp GF181 (Val 'Num GF181)
tempConvert = do
  toFahrenheit <- input
  degree <- input
  return $
    cond
      toFahrenheit
      (degree * 9 / 5 + 32)
      (degree - 32 * 5 / 9)

terminationProblem :: Comp GF181 (Val ('Arr ('Arr 'Bool)) GF181)
terminationProblem = run "A"
  where
    -- Construct a W8 from a Word8
    fromWord8 :: Word8 -> Comp GF181 (Val ('Arr 'Bool) GF181)
    fromWord8 word = toArray $ Prelude.map (Boolean . testBit word) [0 .. 7]

    -- Construct a W8 from a Char
    fromChar :: Char -> Comp GF181 (Val ('Arr 'Bool) GF181)
    fromChar = fromWord8 . toEnum . fromEnum

    -- Construct an array of W8s from a String
    run :: String -> Comp GF181 (Val ('Arr ('Arr 'Bool)) GF181)
    run xs = mapM fromChar xs >>= toArray

-- |
main :: IO ()
main = return ()

-- -- here goes the program you want to compile
-- let program = assertToBe42

-- let toR1CS = False
-- if toR1CS
--   then compileAsR1CS program -- compile as a R1CS
--   else compile program -- compile as a ConstraintSystem

-- assertArrayToBe42 :: Comp GF181 (Val 'Unit GF181)
-- assertArrayToBe42 = do
--   let len = 8

--   xs <- inputs len

--   forM_ [0 .. len - 1] $ \i -> do
--     x <- access xs i
--     assert $ Var x `Eq` 3210

--   return unit

-- -- | A program that outputs the square of its input
-- square :: Comp GF181 (Val 'Num GF181)
-- square = do
--   x <- input
--   return (Var x * Var x)

assertToBe42 :: Comp GF181 (Val 'Unit GF181)
assertToBe42 = do
  x <- input
  assert (x `Eq` 42)
  return unit

-- | A program that expects the second input to be the square of the first input
-- This program returns no output (hence 'return unit')
assertSquare :: Comp GF181 (Val 'Unit GF181)
assertSquare = do
  x <- input
  y <- input
  assert ((x * x) `Eq` y)
  return unit

loop3 :: Int -> Int -> Comp GF181 (Val 'Unit GF181)
loop3 n m = do
  xs <- inputs2 n m
  -- expecting square of signatures as input
  squares <- inputs2 n m
  -- for each signature
  forM_ [0 .. n - 1] $ \i -> do
    -- for each term of signature
    forM_ [0 .. m - 1] $ \j -> do
      x <- access2 xs (i, j)
      x' <- access2 squares (i, j)
      assert (x' `Eq` (x * x))

  return unit

--   --------------------------------------------------------------------------------

--   -- loop1 :: Comp GF181 (Val 'Unit GF181)
--   -- loop1 = do
--   --   xs <- allocArray 4
--   --   -- iterate through the array and assert them all to be 0
--   --   forM_ [0 .. 3] $ \i -> do
--   --     update xs i 43
--   --     x <- access xs i
--   --     assert (Var x `Eq` 42)

--   -- forM_ [0 .. 3] $ \i -> do
--   --   update xs i 43
--   -- assert (Var x `Eq` 43)

--   return unit

-- loop2 :: Comp GF181 (Val 'Unit GF181)
-- loop2 = do
--   x <- inputNum
--   ys <- inputArray 4
--   -- iterate through the array and reassign their value to 'x'
--   forM_ [0 .. 3] $ \i -> do
--     update ys i (Var x)

--   return unit

-- loop3 :: Comp GF181 (Val 'Unit GF181)
-- loop3 = do
--   xs <- inputArray 4
--   -- iterate through the array and assert them all to be 0
--   loopi xs $ \_ x -> do
--     assert (Var x `Eq` 0)

--   return unit

-- -- reduce1 :: Comp GF181 (Val 'Num GF181)
-- -- reduce1 = do
-- --   xs <- inputArray 4
-- --   -- aggregate all variables in xs
-- --   reducei xs 4 8 $ \_ acc x -> do
-- --     return (acc + Var x)

-- --------------------------------------------------------------------------------

-- loop1 :: Comp GF181 (Val 'Unit GF181)
-- loop1 = do
--   -- xs <- inputArray 2  :: Comp GF181 (Ref ('A ('V 'Num)))
--   -- ys <- inputArray 2  :: Comp GF181 (Ref ('A ('V 'Num)))
--   -- zs <- allocArray' [4, 5] :: Comp GF181 (Ref ('A ('V 'Num)))
--   -- ws <- expose zs >>= allocArray'

--   -- iterate through the array and assert them all to be 0
--   -- forM_ [0 .. 2] $ \_ -> do
--     -- update (xs :: Ref ('A ('V 'Num))) i 43
--     -- x <- access i xs
--     -- y <- access i ys
--     -- assert

--     -- assertArrayEqual 2 xs zs
--   -- assertArrayEqual 2 xs ws
--   -- assertArrayEqual 2 ws ys

--   return unit