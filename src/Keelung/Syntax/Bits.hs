{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Like `Data.Bits` but with `Boolean` instead of `Bool`
module Keelung.Syntax.Bits where

-- import qualified Data.Bits
-- import Data.Field.Galois

import GHC.TypeNats (KnownNat)
import Keelung.Syntax

infixl 9 !!!
infixl 8 .&.
infixl 7 .|.

class Bits a where
  -- {-# MINIMAL bitWidth #-}
  -- bitWidth :: a -> Int

  -- | Bitwise \"and\"
  (.&.) :: a -> a -> a

  -- | Bitwise \"or\"
  (.|.) :: a -> a -> a

  -- | Bitwise \"xor\"
  (.^.) :: a -> a -> a

  -- | Retrieve the i-th bit and return it as Boolean
  --   The LSB is the 0-th bit and the MSB is the (n-1)-th bit
  --      where n is the number of bits of the Number
  --   You can access the MSB with (-1) because the index is modulo n
  (!!!) :: a -> Int -> Boolean

  -- pack :: [Boolean] -> a

instance Bits Number where
  (.&.) = AndNum
  (.|.) = OrNum
  (.^.) = XorNum
  (!!!) = NumBit

instance Bits Boolean where
  (.&.) = And
  (.|.) = Or
  (.^.) = Xor
  x !!! _ = x

instance KnownNat w => Bits (UInt w) where
  (.&.) = AndUInt
  (.|.) = OrUInt
  (.^.) = XorUInt
  (!!!) = UIntBit

-- testBit :: a -> Int -> Boolean
-- testBit x i = if Data.Bits.testBit (toInteger x) (i `mod` bitSize x) then true else false

-- -- | All instances of Galois fields are also instances of `Bits`
-- --   `bitSize` will have to be calculated at runtime every time though,
-- --   It's recommended to declare specialized instances for each Galois fields
-- instance {-# OVERLAPPABLE #-} (GaloisField a, Integral a) => Bits a where
--   bitSize x = go 0 (order x)
--     where
--       go i n = if n == 0 then i else go (i + 1) (Data.Bits.unsafeShiftR n 1)

-- -- | Specialized instance for `B64`
-- instance {-# INCOHERENT #-} Bits B64 where
--   bitSize = const 64

-- -- | Specialized instance for `GF181`
-- instance {-# INCOHERENT #-} Bits GF181 where
--   bitSize = const 181

-- -- | Specialized instance for `BN128`
-- instance {-# INCOHERENT #-} Bits BN128 where
--   bitSize = const 254

-- --------------------------------------------------------------------------------

-- toBits :: (GaloisField a, Integral a) => a -> [Boolean]
-- toBits x = map (testBit x) [0 .. bitSize x - 1]