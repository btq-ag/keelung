{-# LANGUAGE DataKinds #-}

module Main where

import Keelung 

main :: IO ()
main = do
    generateAs "square.keel" square

square :: Comp GF181 (Expr 'Unit GF181)
square = do 
    x <- inputVar
    y <- inputVar
    assert $ (Var x * Var x) `Eq` Var y


    return unit 

