{-# LANGUAGE DataKinds #-}

module Main where

import Test.Hspec
import Test.VarLayout qualified as VarBookkeep

main :: IO ()
main = hspec $ do
  describe "Variable Bookkeeping" VarBookkeep.tests
