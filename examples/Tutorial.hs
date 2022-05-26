{-# LANGUAGE DataKinds #-}

module Main where

import Keelung

-- |
main :: IO ()
main = do
  -- here goes the program you want to compile
  let program = assertToBe42

  let toR1CS = False
  if toR1CS
    then compileAsR1CS program -- compile as a R1CS
    else compile program -- compile as a ConstraintSystem

-- | A program that outputs the square of its input 
-- square :: Comp GF181 (Expr 'Num GF181)
-- square = do
--   x <- inputVar
--   return $ Var x * Var x

-- | A program that expects the second input to be the square of the first input 
-- This program returns no output (hence 'return unit')
assertSquare :: Comp GF181 (Expr 'Unit GF181)
assertSquare = do
  x <- inputVar
  y <- inputVar
  assert $ (Var x * Var x) `Eq` Var y
  return unit

assertToBe42 :: Comp GF181 (Expr 'Unit GF181)
assertToBe42 = do
  x <- inputVar
  assert $ Var x `Eq` 42 
  return unit

    
