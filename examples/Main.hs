{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Main where

import Control.Monad (forM_)
import Keelung

main = return ()

-- -- | Outputs whether number is given.
-- echo :: Comp GF181 (Expr 'Num GF181)
-- echo = do
--   x <- inputVar -- request for an input and bind it to 'x'
--   return $ Var x -- return 'x'

-- -- | A program that expects 2 inputs and returns no output
-- useless :: Comp GF181 (Expr 'Unit GF181)
-- useless = do
--   _x <- inputVar -- request for an input and bind it to 'x'
--   _y <- inputVar -- request for an input and bind it to 'y'
--   return unit -- return nothing

-- -- Formula: (0°C × 9/5) + 32 = 32°F
-- tempConvert :: Comp GF181 (Expr 'Num GF181)
-- tempConvert = do
--   toFahrenheit <- inputVar
--   degree <- inputVar
--   return $
--     If
--       (Var toFahrenheit)
--       (Var degree * 9 / 5 + 32)
--       (Var degree - 32 * 5 / 9)

-- -- |
-- main :: IO ()
-- main = do
--   -- here goes the program you want to compile
--   let program = assertToBe42

--   let toR1CS = False
--   if toR1CS
--     then compileAsR1CS program -- compile as a R1CS
--     else compile program -- compile as a ConstraintSystem

-- assertArrayToBe42 :: Comp GF181 (Expr 'Unit GF181)
-- assertArrayToBe42 = do
--   let len = 8

--   xs <- inputArray len

--   forM_ [0 .. len - 1] $ \i -> do
--     x <- access xs i 
--     assert $ Var x `Eq` 3210

--   return unit

-- -- | A program that outputs the square of its input
-- square :: Comp GF181 (Expr 'Num GF181)
-- square = do
--   x <- inputVar
--   return (Var x * Var x)

-- assertToBe42 :: Comp GF181 (Expr 'Unit GF181)
-- assertToBe42 = do
--   x <- inputVar
--   assert (Var x `Eq` 42)
--   return unit

-- -- | A program that expects the second input to be the square of the first input
-- -- This program returns no output (hence 'return unit')
-- assertSquare :: Comp GF181 (Expr 'Unit GF181)
-- assertSquare = do
--   x <- inputVar
--   y <- inputVar
--   assert ((Var x * Var x) `Eq` Var y)
--   -- return unit

--   --------------------------------------------------------------------------------

--   -- loop1 :: Comp GF181 (Expr 'Unit GF181)
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

-- loop2 :: Comp GF181 (Expr 'Unit GF181)
-- loop2 = do
--   x <- inputVarNum
--   ys <- inputArray 4
--   -- iterate through the array and reassign their value to 'x'
--   forM_ [0 .. 3] $ \i -> do
--     update ys i (Var x)

--   return unit

-- loop3 :: Comp GF181 (Expr 'Unit GF181)
-- loop3 = do
--   xs <- inputArray 4
--   -- iterate through the array and assert them all to be 0
--   loopi xs $ \_ x -> do
--     assert (Var x `Eq` 0)

--   return unit

-- -- reduce1 :: Comp GF181 (Expr 'Num GF181)
-- -- reduce1 = do
-- --   xs <- inputArray 4
-- --   -- aggregate all variables in xs
-- --   reducei xs 4 8 $ \_ acc x -> do
-- --     return (acc + Var x)

-- --------------------------------------------------------------------------------

-- loop1 :: Comp GF181 (Expr 'Unit GF181)
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