{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Like `Data.Bits` but with `Boolean` instead of `Bool`
module Keelung.Data.Bits where

import GHC.TypeNats (KnownNat)
import Keelung.Syntax

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

  -- | Rotates left, extends less significant bits with 0
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

shiftL :: Bits a => a -> Int -> a
shiftL = shift

shiftR :: Bits a => a -> Int -> a
shiftR x i = shiftR x (-i)

-- | Infix version of 'shiftR'.
--
-- @since 4.17
(.>>.) :: (Bits a) => a -> Int -> a
(.>>.) = shiftR

infixl 8 .>>.

-- | Infix version of 'shiftL'.
--
-- @since 4.17
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