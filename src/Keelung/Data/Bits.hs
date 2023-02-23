{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Like `Data.Bits` but with `Boolean` instead of `Bool`
module Keelung.Data.Bits where

import Data.Bits qualified as Bits
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.TypeNats (KnownNat)
import Keelung.Syntax

-- | Bitwise operations on Keelung values
class Bits a where
  -- {-# MINIMAL bitWidth #-}
  -- bitWidth :: a -> Int

  -- | Bitwise \"and\"
  (.&.) :: a -> a -> a

  infixl 8 .&.

  -- | Bitwise \"or\"
  (.|.) :: a -> a -> a

  infixl 7 .|.

  -- | Bitwise \"xor\"
  (.^.) :: a -> a -> a

  infixl 6 .^.

  -- | Rotates left by @i@ bits if @i@ is positive, or right by @-i@ bits otherwise.
  rotate :: a -> Int -> a

  infixl 8 `rotate`

  -- | @'shift' x i@ shifts @x@ left by @i@ bits if @i@ is positive, or right by @-i@ bits otherwise. Vacated bits are filled with 0.
  shift :: a -> Int -> a

  infixl 8 `shift`

  -- | Retrieve the i-th bit and return it as Boolean
  --   The LSB is the 0-th bit and the MSB is the (n-1)-th bit
  --      where n is the bit width
  --   You can access the MSB with (-1) because the index is modulo n
  (!!!) :: a -> Int -> Boolean

  infixl 9 !!!

  -- | Reverse all the bits in the argument
  complement :: a -> a

-- | Synonym for 'shift'
shiftL :: Bits a => a -> Int -> a
shiftL = shift

-- | Opposite of 'shiftL'
shiftR :: Bits a => a -> Int -> a
shiftR x i = shiftR x (-i)

-- | Infix version of 'shiftR'.
(.>>.) :: (Bits a) => a -> Int -> a
(.>>.) = shiftR

infixl 8 .>>.

-- | Infix version of 'shiftL'.
(.<<.) :: (Bits a) => a -> Int -> a
(.<<.) = shiftL

infixl 8 .<<.

instance Bits Boolean where
  (.&.) = And
  (.|.) = Or
  (.^.) = Xor
  rotate x _ = x
  shift x 0 = x
  shift _ _ = false
  x !!! _ = x
  complement = Not

instance KnownNat w => Bits (UInt w) where
  (.&.) = AndU
  (.|.) = OrU
  (.^.) = XorU
  rotate expr i = RoLU (widthOf expr) i expr
  shift expr i = ShLU (widthOf expr) i expr
  (!!!) = BitU
  complement = NotU

-- | Make 'Word8' an instance of 'Bits'
instance Bits Word8 where
  (.&.) = (Bits..&.)
  (.|.) = (Bits..|.)
  (.^.) = Bits.xor
  rotate = Bits.rotate
  shift = Bits.shift
  x !!! i = Boolean (Bits.testBit x i)
  complement = Bits.complement

-- | Make 'Word16' an instance of 'Bits'
instance Bits Word16 where
  (.&.) = (Bits..&.)
  (.|.) = (Bits..|.)
  (.^.) = Bits.xor
  rotate = Bits.rotate
  shift = Bits.shift
  x !!! i = Boolean (Bits.testBit x i)
  complement = Bits.complement

-- | Make 'Word32' an instance of 'Bits'
instance Bits Word32 where
  (.&.) = (Bits..&.)
  (.|.) = (Bits..|.)
  (.^.) = Bits.xor
  rotate = Bits.rotate
  shift = Bits.shift
  x !!! i = Boolean (Bits.testBit x i)
  complement = Bits.complement

-- | Make 'Word64' an instance of 'Bits'
instance Bits Word64 where
  (.&.) = (Bits..&.)
  (.|.) = (Bits..|.)
  (.^.) = Bits.xor
  rotate = Bits.rotate
  shift = Bits.shift
  x !!! i = Boolean (Bits.testBit x i)
  complement = Bits.complement
