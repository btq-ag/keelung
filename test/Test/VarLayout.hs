{-# LANGUAGE LambdaCase #-}
module Test.VarLayout (tests, run) where

import Keelung
import Keelung.Syntax.Counters
import Keelung.Constraint.R1CS
import Test.HUnit (assertFailure)
import Test.Hspec

run :: IO ()
run = hspec tests

execute :: Encode t => Comp t -> IO Counters
execute program = do
  compile gf181 program >>= \case
    Left err -> assertFailure $ show err
    Right r1cs -> return $ r1csCounters r1cs

tests :: SpecWith ()
tests = do
  describe "Variable indexing 0" $ do
    --
    --                F   B   BR  U
    --       output   3   0   0   0
    --        input   2   1   0   0
    -- intermediate   0   0   20  5
    --
    let counters =
          ( addCount (PublicInput, WriteBool) 1
              . addCount (PublicInput, WriteField) 2
              . addCount (Output, WriteField) 3
              . addCount (Intermediate, WriteUInt 4) 5
          )
            mempty
    it "reindex" $ do
      reindex counters Output ReadField 0 `shouldBe` 0
      reindex counters Output ReadField 1 `shouldBe` 1
      reindex counters Output ReadField 2 `shouldBe` 2
      reindex counters PublicInput ReadField 0 `shouldBe` 3
      reindex counters PublicInput ReadField 1 `shouldBe` 4
      reindex counters PublicInput ReadBool 0 `shouldBe` 5
      reindex counters Intermediate (ReadUInt 4) 0 `shouldBe` 6
      reindex counters Intermediate (ReadUInt 4) 1 `shouldBe` 10
      reindex counters Intermediate (ReadUInt 4) 2 `shouldBe` 14
      reindex counters Intermediate (ReadUInt 4) 3 `shouldBe` 18
      reindex counters Intermediate (ReadUInt 4) 4 `shouldBe` 22

    it "getCount" $ do
      getCount counters (Output, ReadField) `shouldBe` 3
      getCount counters (Output, ReadBool) `shouldBe` 0
      getCount counters (PublicInput, ReadField) `shouldBe` 2
      getCount counters (PublicInput, ReadBool) `shouldBe` 1
      getCount counters (Intermediate, ReadField) `shouldBe` 0
      getCount counters (Intermediate, ReadBool) `shouldBe` 0
      getCount counters (Intermediate, ReadUInt 3) `shouldBe` 0
      getCount counters (Intermediate, ReadUInt 4) `shouldBe` 5

  describe "Variable indexing 1" $ do
    --
    --                F   B   BR  U
    --       output   0   6   0   0
    --        input   0   0   4   1
    -- intermediate   0   0   0   0
    --
    -- bitTestVarUI :: Comp (Arr Boolean)
    -- bitTestVarUI = do
    --   x <- inputUInt @4
    --   return $ toArray [x !!! (-1), x !!! 0, x !!! 1, x !!! 2, x !!! 3, x !!! 4]
    let counters =
          ( addCount (PublicInput, WriteUInt 4) 1
              . addCount (Output, WriteBool) 6
          )
            mempty
    it "reindex" $ do
      reindex counters Output ReadBool 0 `shouldBe` 0
      reindex counters Output ReadBool 1 `shouldBe` 1
      reindex counters Output ReadBool 2 `shouldBe` 2
      reindex counters Output ReadBool 3 `shouldBe` 3
      reindex counters Output ReadBool 4 `shouldBe` 4
      reindex counters Output ReadBool 5 `shouldBe` 5
      reindex counters PublicInput (ReadUInt 4) 0 `shouldBe` 6

    it "getCount" $ do
      getCount counters (Output, ReadField) `shouldBe` 0
      getCount counters (Output, ReadBool) `shouldBe` 6
      getCount counters (PublicInput, ReadField) `shouldBe` 0
      getCount counters (PublicInput, ReadBool) `shouldBe` 0
      getCount counters (PublicInput, ReadUInt 3) `shouldBe` 0
      getCount counters (PublicInput, ReadUInt 4) `shouldBe` 1
      getCount counters (Intermediate, ReadField) `shouldBe` 0
      getCount counters (Intermediate, ReadBool) `shouldBe` 0
      getCount counters (Intermediate, ReadUInt 3) `shouldBe` 0
      getCount counters (Intermediate, ReadUInt 4) `shouldBe` 0
      getCount counters (Intermediate, ReadUInt 5) `shouldBe` 0

  describe "Variable indexing 2" $ do
    --
    --                F   B   BR  U
    --       output   0   0   4   1
    --        input   0   0   8   2
    -- intermediate   0   0   4   1
    --
    let counters =
          ( addCount (Output, WriteUInt 4) 1
              . addCount (PublicInput, WriteUInt 4) 2
              . addCount (Intermediate, WriteUInt 4) 1
          )
            mempty
    it "reindex" $ do
      reindex counters Output (ReadUInt 4) 0 `shouldBe` 0
      reindex counters PublicInput (ReadUInt 4) 0 `shouldBe` 4
      reindex counters PublicInput (ReadUInt 4) 1 `shouldBe` 8
      reindex counters Intermediate (ReadUInt 4) 0 `shouldBe` 12

  describe "Variable indexing 3" $ do
    --
    --                F   B   BR  U4  U5  U8
    --       output   0   0   0   0   0   0
    --        input   0   0   0   0   0   0
    -- intermediate   0   0   21  4   1   1
    --
    let counters =
          ( addCount (Intermediate, WriteUInt 4) 2
              . addCount (Intermediate, WriteUInt 5) 1
              . addCount (Intermediate, WriteUInt 8) 1
          )
            mempty
    it "reindex" $ do
      reindex counters Intermediate (ReadUInt 4) 0 `shouldBe` 0
      reindex counters Intermediate (ReadUInt 4) 1 `shouldBe` 4
      reindex counters Intermediate (ReadUInt 5) 0 `shouldBe` 8
      reindex counters Intermediate (ReadUInt 8) 0 `shouldBe` 13

  describe "Variable indexing 4" $ do
    --
    --                F   B   BR  U4  U5
    --       output   0   0   4   1   0
    --        input   0   0   12  3   0
    -- intermediate   0   0   0   3   4
    --
    let counters =
          ( addCount (Intermediate, WriteUInt 4) 3
              . addCount (Intermediate, WriteUInt 5) 4
              . addCount (Output, WriteUInt 4) 1
              . addCount (PublicInput, WriteUInt 4) 3
          )
            mempty
    it "reindex" $ do
      reindex counters Output (ReadUInt 4) 0 `shouldBe` 0
      reindex counters PublicInput (ReadUInt 4) 0 `shouldBe` 4
      reindex counters PublicInput (ReadUInt 4) 1 `shouldBe` 8
      reindex counters PublicInput (ReadUInt 4) 2 `shouldBe` 12
      reindex counters Intermediate (ReadUInt 4) 0 `shouldBe` 16
      reindex counters Intermediate (ReadUInt 4) 1 `shouldBe` 20
      reindex counters Intermediate (ReadUInt 4) 2 `shouldBe` 24
      reindex counters Intermediate (ReadUInt 5) 0 `shouldBe` 28
      reindex counters Intermediate (ReadUInt 5) 1 `shouldBe` 33
      reindex counters Intermediate (ReadUInt 5) 2 `shouldBe` 38
      reindex counters Intermediate (ReadUInt 5) 3 `shouldBe` 43

  describe "Layout 0" $ do
    --                F   B   BR  U
    --       output   1   0   0   0
    --        input   3   0   0   0
    -- intermediate   0   0   0   0
    let program = do
          x <- inputField Public
          y <- inputField Public
          z <- inputField Public
          return $ x + y + z

    it "reindex" $ do
      counters <- execute program
      reindex counters Output ReadField 0 `shouldBe` 0
      reindex counters PublicInput ReadField 0 `shouldBe` 1
      reindex counters PublicInput ReadField 1 `shouldBe` 2
      reindex counters PublicInput ReadField 2 `shouldBe` 3

  describe "Layout 1" $ do
    --                F   B   BR  U
    --       output   1   0   0   0
    --        input   1   1   0   0
    -- intermediate   0   0   0   0
    let program = do
          x <- inputField Public
          y <- inputBool Public
          return $ cond y x 0
    it "reindex" $ do
      counters <- execute program
      reindex counters Output ReadField 0 `shouldBe` 0
      reindex counters PublicInput ReadField 0 `shouldBe` 1
      reindex counters PublicInput ReadBool 0 `shouldBe` 2
