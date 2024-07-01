-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
module Test.Monad where

-- import Keelung
-- import Keelung.Monad
-- import Test.HUnit (assertFailure)
-- import Test.Hspec
-- import GHC.Generics hiding (UInt)


-- run :: IO ()
-- run = do
--   putStrLn "compiling program with custom datatype input"
--   hspec $ before (compile gf181 testPerson) _

-- -- Tests for generic datatype inputs
-- data Person = Person { age :: UInt 8, check :: (Boolean, (Field, UInt 8)) }
--   deriving Generic

-- instance Inputable Person

-- testPerson :: Comp ()
-- testPerson = do
--    (p :: Person) <- inputData Public 
--    assert $ (age p) `gt` (UInt 18)
--    assert $ (fst $ check p) `eq` (Boolean True)

-- tests :: SpecWith ()
-- tests = do
--   describe "Generic datatype input" $ do
--     it "Assertions on record fields" $ do
--       shouldBe _ _
        