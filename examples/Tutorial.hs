{-# LANGUAGE DataKinds #-}

module Main where

import Keelung 

main :: IO ()
main = do
    compile square

square :: Comp GF181 (Expr 'Unit GF181)
square = do 
    x <- inputVar
    y <- inputVar
    assert $ (Var x * Var x) `Eq` Var y
    return unit 

