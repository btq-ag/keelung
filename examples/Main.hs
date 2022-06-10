{-# LANGUAGE DataKinds #-}

module Main where

import Control.Monad (forM_)
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

assertToBe42 :: Comp GF181 (Expr 'Unit GF181)
assertToBe42 = do
  x <- inputVar
  assert $ Var x `Eq` 42
  return unit

assertArrayToBe42 :: Comp GF181 (Expr 'Unit GF181)
assertArrayToBe42 = do
  let len = 8

  xs <- inputArray len

  forM_ [0 .. len - 1] $ \i -> do
    x <- access xs i
    assert $ Var x `Eq` 3210

  return unit

-- | A program that outputs the square of its input
square :: Comp GF181 (Expr 'Num GF181)
square = do
  x <- inputVar
  return (Var x * Var x)

-- | A program that expects the second input to be the square of the first input
-- This program returns no output (hence 'return unit')
assertSquare :: Comp GF181 (Expr 'Unit GF181)
assertSquare = do
  x <- inputVar
  y <- inputVar
  assert ((Var x * Var x) `Eq` Var y)
  return unit

--------------------------------------------------------------------------------

loop1 :: Comp GF181 (Expr 'Unit GF181)
loop1 = do
  xs <- allocArray 4
  -- iterate through the array and assert them all to be 0
  forM_ [0 .. 3] $ \i -> do
    x <- access xs i 
    assert (Var x `Eq` 0)
  return unit 

loop2 :: Comp GF181 (Expr 'Unit GF181)
loop2 = do
  x <- inputVarNum
  ys <- allocArray 4
  -- iterate through the array and reassign their value to 'x'
  forM_ [0 .. 3] $ \i -> do
    update ys i (Var x)

  return unit 

loop3 :: Comp GF181 (Expr 'Unit GF181)
loop3 = do
  xs <- inputArray 4
  -- iterate through the array and assert them all to be 0
  loop xs 4 $ \x -> do 
    assert (Var x `Eq` 0)
  
  return unit 

reduce1 :: Comp GF181 (Expr 'Num GF181)
reduce1 = do
  xs <- inputArray 4
  -- aggregate all variables in xs 
  reduce xs 4 0 $ \acc x -> do 
    return (acc + Var x)


-- test :: Comp GF181 (Expr 'Num GF181)
-- test = do
--   x <- inputVar :: Comp GF181 (Ref ('V 'Num))

--   ys <- allocArray 4

--   forM_ [0 .. 3] $ \i -> do
--     update ys i (Var x)

--   product' ys 4 

--   -- let add acc i = do
--   --       y <- access ys i
--   --       return (acc + Var y)

--   -- foldM
--   --   add
--   --   4
--   --   [0 .. 3]
