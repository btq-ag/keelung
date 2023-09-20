{-# LANGUAGE DuplicateRecordFields #-}
module Keelung.CircuitFormat
  ( Format(..),
    R1CSBinHeader(..),
    WtnsBinHeader(..),
  )
where

-- | Format indicator for R1CS/witness generation,
--   `Aurora` for BTQ's Aurora implementation, and `Circom`
--   for Circom/Snarkjs' binary R1CS (.r1cs) and witness (.wtns) format.
data Format = Aurora | Snarkjs

data R1CSBinHeader = R1CSBinHeader {
    prime        :: Integer
,   nWires       :: Int
,   nPubOut      :: Int
,   nPubIn       :: Int
,   nPrvIn       :: Int
,   nLabels      :: Int
,   mConstraints :: Int
}

data WtnsBinHeader = WtnsBinHeader {
    prime :: Integer
,   nWtns :: Int
}