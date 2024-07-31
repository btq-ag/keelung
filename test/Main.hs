{-# LANGUAGE DataKinds #-}

module Main where

import Test.Hspec
import Test.VarLayout qualified as VarBookkeep
import Test.Snarkjs qualified as Snarkjs

main :: IO ()
main = hspec $ do
  describe "Variable Bookkeeping" VarBookkeep.tests
  describe "Snarkjs Integration" Snarkjs.tests
