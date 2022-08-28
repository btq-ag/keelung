-- | How to profile the program:
--  1.  `stack build --profile keelung:bench:profile` 
--   If you have "ghc-prof-flamegraph" available, you can generate flamegraphs from this 
--  2.  `cat profile.prof | ghc-prof-flamegraph > profile.prof.svg`

{-# LANGUAGE DataKinds #-}
module Main where

import Data.Bits (Bits (testBit))
import Data.Word (Word8)
import Keelung
import Control.DeepSeq (NFData(rnf))
import Control.Exception (evaluate)

-- |
main :: IO ()
main = evaluate $ rnf $ elaborate (return $ fromString' (string 200000))
  where

    -- | `fromWord8` implemented with immutable arrays
    fromWord8' :: Word8 -> Val ('Arr 'Bool) n
    fromWord8' word = toArrayI $ Prelude.map (Boolean . testBit word) [0 .. 7]

    -- | `fromChar` implemented with immutable arrays
    fromChar' :: Char -> Val ('Arr 'Bool) n
    fromChar' = fromWord8' . toEnum . fromEnum

    -- | `fromString` implemented with immutable arrays
    fromString' :: String -> Val ('Arr ('Arr 'Bool)) GF181 
    fromString' = toArrayI . map fromChar'

    string :: Int -> String
    string n = concat $ replicate n "Hello world"
