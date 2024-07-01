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
    --                F   B   U4
    --       output   3   0   0
    --    pub input   2   1   0
    --   priv input   0   0   0
    -- intermediate   0   0   5
    --
    let counters =
          ( addCount (PublicInput, WriteBool) 1
              . addCount (PublicInput, WriteField) 2
              . addCount (Output, WriteField) 3
              . addCount (Intermediate, WriteUInt 4) 5
          )
            mempty
    it "getOffset" $ do
      getOffset counters (Output, ReadField) `shouldBe` 0
      getOffset counters (Output, ReadBool) `shouldBe` 3
      getOffset counters (Output, ReadAllUInts) `shouldBe` 3
      getOffset counters (Output, ReadUInt 4) `shouldBe` 3
      getOffset counters (PublicInput, ReadField) `shouldBe` 3
      getOffset counters (PublicInput, ReadBool) `shouldBe` 5
      getOffset counters (PublicInput, ReadAllUInts) `shouldBe` 6
      getOffset counters (PublicInput, ReadUInt 4) `shouldBe` 6
      getOffset counters (PrivateInput, ReadField) `shouldBe` 6
      getOffset counters (PrivateInput, ReadBool) `shouldBe` 6
      getOffset counters (PrivateInput, ReadAllUInts) `shouldBe` 6
      getOffset counters (PrivateInput, ReadUInt 4) `shouldBe` 6
      getOffset counters (Intermediate, ReadField) `shouldBe` 6
      getOffset counters (Intermediate, ReadBool) `shouldBe` 6
      getOffset counters (Intermediate, ReadAllUInts) `shouldBe` 6
      getOffset counters (Intermediate, ReadUInt 4) `shouldBe` 6

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
    --                F   B   U4
    --       output   0   6   0
    --    pub input   0   0   1
    --   priv input   0   0   0
    -- intermediate   0   0   0
    --
    let counters =
          ( addCount (PublicInput, WriteUInt 4) 1
              . addCount (Output, WriteBool) 6
          )
            mempty
    it "getOffset" $ do
      getOffset counters (Output, ReadField) `shouldBe` 0
      getOffset counters (Output, ReadBool) `shouldBe` 0
      getOffset counters (Output, ReadAllUInts) `shouldBe` 6
      getOffset counters (Output, ReadUInt 4) `shouldBe` 6
      getOffset counters (PublicInput, ReadField) `shouldBe` 6
      getOffset counters (PublicInput, ReadBool) `shouldBe` 6
      getOffset counters (PublicInput, ReadAllUInts) `shouldBe` 6
      getOffset counters (PublicInput, ReadUInt 4) `shouldBe` 6
      getOffset counters (PrivateInput, ReadField) `shouldBe` 10
      getOffset counters (PrivateInput, ReadBool) `shouldBe` 10
      getOffset counters (PrivateInput, ReadAllUInts) `shouldBe` 10
      getOffset counters (PrivateInput, ReadUInt 4) `shouldBe` 10
      getOffset counters (Intermediate, ReadField) `shouldBe` 10
      getOffset counters (Intermediate, ReadBool) `shouldBe` 10
      getOffset counters (Intermediate, ReadAllUInts) `shouldBe` 10
      getOffset counters (Intermediate, ReadUInt 4) `shouldBe` 10

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
    --                F   B   U4  U6
    --       output   0   0   1   0
    --    pub input   0   0   2   1
    --   priv input   0   0   0   2
    -- intermediate   0   0   1   3
    --
    let counters =
          ( addCount (Output, WriteUInt 4) 1
              . addCount (PublicInput, WriteUInt 4) 2
              . addCount (Intermediate, WriteUInt 4) 1
              . addCount (Intermediate, WriteUInt 6) 1
              . addCount (Intermediate, WriteUInt 6) 3
              . addCount (PrivateInput, WriteUInt 6) 2
              . addCount (PublicInput, WriteUInt 6) 1
          )
            mempty

    it "getOffset" $ do
      getOffset counters (Output, ReadField) `shouldBe` 0
      getOffset counters (Output, ReadBool) `shouldBe` 0
      getOffset counters (Output, ReadAllUInts) `shouldBe` 0
      getOffset counters (Output, ReadUInt 4) `shouldBe` 0
      getOffset counters (Output, ReadUInt 6) `shouldBe` 4
      getOffset counters (PublicInput, ReadField) `shouldBe` 4
      getOffset counters (PublicInput, ReadBool) `shouldBe` 4
      getOffset counters (PublicInput, ReadAllUInts) `shouldBe` 4
      getOffset counters (PublicInput, ReadUInt 4) `shouldBe` 4
      getOffset counters (PublicInput, ReadUInt 6) `shouldBe` 12
      getOffset counters (PrivateInput, ReadField) `shouldBe` 18
      getOffset counters (PrivateInput, ReadBool) `shouldBe` 18
      getOffset counters (PrivateInput, ReadAllUInts) `shouldBe` 18
      getOffset counters (PrivateInput, ReadUInt 4) `shouldBe` 18
      getOffset counters (PrivateInput, ReadUInt 6) `shouldBe` 18
      getOffset counters (Intermediate, ReadField) `shouldBe` 30
      getOffset counters (Intermediate, ReadBool) `shouldBe` 30
      getOffset counters (Intermediate, ReadAllUInts) `shouldBe` 30
      getOffset counters (Intermediate, ReadUInt 4) `shouldBe` 30
      getOffset counters (Intermediate, ReadUInt 6) `shouldBe` 34

  describe "Variable indexing 3" $ do
    --
    --                F   B   U4  U5
    --       output   0   0   1   0
    --    pub input   0   0   3   0
    --   priv input   0   0   0   4
    -- intermediate   0   0   3   0
    --
    let counters =
          ( addCount (Intermediate, WriteUInt 4) 3
              . addCount (PrivateInput, WriteUInt 5) 4
              . addCount (Output, WriteUInt 4) 1
              . addCount (PublicInput, WriteUInt 4) 3
          )
            mempty
    it "getOffset" $ do 
      getOffset counters (Output, ReadField) `shouldBe` 0
      getOffset counters (Output, ReadBool) `shouldBe` 0
      getOffset counters (Output, ReadAllUInts) `shouldBe` 0
      getOffset counters (Output, ReadUInt 4) `shouldBe` 0
      getOffset counters (Output, ReadUInt 5) `shouldBe` 4
      getOffset counters (PublicInput, ReadField) `shouldBe` 4
      getOffset counters (PublicInput, ReadBool) `shouldBe` 4
      getOffset counters (PublicInput, ReadAllUInts) `shouldBe` 4
      getOffset counters (PublicInput, ReadUInt 4) `shouldBe` 4
      getOffset counters (PublicInput, ReadUInt 5) `shouldBe` 16
      getOffset counters (PrivateInput, ReadField) `shouldBe` 16
      getOffset counters (PrivateInput, ReadBool) `shouldBe` 16
      getOffset counters (PrivateInput, ReadAllUInts) `shouldBe` 16
      getOffset counters (PrivateInput, ReadUInt 4) `shouldBe` 16
      getOffset counters (PrivateInput, ReadUInt 5) `shouldBe` 16
      getOffset counters (Intermediate, ReadField) `shouldBe` 36
      getOffset counters (Intermediate, ReadBool) `shouldBe` 36
      getOffset counters (Intermediate, ReadAllUInts) `shouldBe` 36
      getOffset counters (Intermediate, ReadUInt 4) `shouldBe` 36
      getOffset counters (Intermediate, ReadUInt 5) `shouldBe` 48

  describe "Layout 0" $ do
    --                F   B   U
    --       output   1   0   0
    --    pub input   3   0   0
    --   priv input   0   0   0
    -- intermediate   0   0   0
    let program = do
          x <- inputField Public
          y <- inputField Public
          z <- inputField Public
          return $ x + y + z

    it "getOffset" $ do 
      counters <- execute program
      getOffset counters (Output, ReadField) `shouldBe` 0
      getOffset counters (Output, ReadBool) `shouldBe` 1
      getOffset counters (Output, ReadAllUInts) `shouldBe` 1
      getOffset counters (PublicInput, ReadField) `shouldBe` 1
      getOffset counters (PublicInput, ReadBool) `shouldBe` 4
      getOffset counters (PublicInput, ReadAllUInts) `shouldBe` 4

  describe "Layout 1" $ do
    --                F   B   U
    --       output   1   0   0
    --    pub input   1   1   0
    -- intermediate   0   0   0
    let program = do
          x <- inputField Public
          y <- inputBool Public
          return $ cond y x 0
    it "getOffset" $ do
      counters <- execute program
      getOffset counters (Output, ReadField) `shouldBe` 0
      getOffset counters (Output, ReadBool) `shouldBe` 1
      getOffset counters (Output, ReadAllUInts) `shouldBe` 1
      getOffset counters (PublicInput, ReadField) `shouldBe` 1
      getOffset counters (PublicInput, ReadBool) `shouldBe` 2
      getOffset counters (PublicInput, ReadAllUInts) `shouldBe` 3

