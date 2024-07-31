{-# LANGUAGE ScopedTypeVariables #-}
module Test.Snarkjs (tests, run) where

import Keelung
import Test.Hspec
import System.Process
import Data.Either (isRight)
import Data.Functor ((<&>))
import GHC.IO.Exception (ExitCode(ExitFailure))
import System.Directory (withCurrentDirectory, createDirectoryIfMissing)

run :: IO ()
run = hspec tests

tests :: SpecWith ()
tests = do
  let inDir = withCurrentDirectory "snarkjs"

  describe "Testing Snarkjs-compatible utilities:" $ do
    _ <- runIO (createDirectoryIfMissing True "snarkjs")
    it "Generating .r1cs files..." $ do
      shouldReturn (inDir (genCircuitBin "echo.r1cs" bn128 echo) <&> isRight) True
      shouldReturn (inDir (genCircuitBin "quad.r1cs" bn128 quad) <&> isRight) True
    it "Generating .wtns files..." $ do
      shouldReturn (inDir (genWtns "echo.wtns" bn128 echo [] [1]) <&> isRight) True
      shouldReturn (inDir (genWtns "quad.wtns" bn128 quad [3,5,-22] [2]) <&> isRight) True

    return ()
  describe "Testing by calling Snarkjs" $ do
    it "Checking if Snarkjs exists..." $ do
      shouldReturn (readProcessWithExitCode "snarkjs" [] [] <&> (\(a,_,_) -> a)) (ExitFailure 99)
    it "Generating .ptau files..." $ do
      shouldReturn (setupSnarkjsPtau 10 "abcd" <&> isRight) True
    it "Generating .zkey..." $ do
      shouldReturn (genZkeyBySnarkjs PLONK "snarkjs/echo.r1cs" "snarkjs/pot10_final.ptau" <&> isRight) True
      shouldReturn (genZkeyBySnarkjs PLONK "snarkjs/quad.r1cs" "snarkjs/pot10_final.ptau" <&> isRight) True
    it "Checking .wtns by Snarkjs..." $ do
      shouldReturn (checkWtnsBySnarkjs "snarkjs/echo.r1cs" "snarkjs/echo.wtns" <&> isRight) True
      shouldReturn (checkWtnsBySnarkjs "snarkjs/quad.r1cs" "snarkjs/quad.wtns" <&> isRight) True
    it "Proving correctness..." $ do
      shouldReturn (proveBySnarkjs PLONK "snarkjs/echo.zkey" "snarkjs/echo.wtns" <&> isRight) True
      shouldReturn (proveBySnarkjs PLONK "snarkjs/quad.zkey" "snarkjs/quad.wtns" <&> isRight) True

-- | A program that outputs whatever number is given.
echo :: Comp Field
echo = do
  x <- input Private -- request for a private input and bind it to 'x'
  return x -- return 'x'

quad :: Comp ()
quad = do
    _a :: Field <- input Public
    _b :: Field <- input Public
    _c :: Field <- input Public
    _x :: Field <- input Private
    assert (eq (_a * _x * _x + _b * _x + _c) 0)
    return ()