{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Keelung.Syntax.Kinded
  ( Number (..),
    Boolean (..),
    Arr (..),
    ArrM (..),
    fromBool,
    toBool,
    true,
    false,
    nbeq,
    neq,
    neg,
    (!!!),
  )
where

import Data.Array.Unboxed (Array)
import Data.Foldable (toList)
import Data.Proxy (Proxy (Proxy))
import Data.Semiring (Ring (..), Semiring (..))
import GHC.TypeNats
import Keelung.Types

infixl 9 !!!

--------------------------------------------------------------------------------

-- | Numbers
data Number
  = Integer Integer -- Integers
  | Rational Rational -- Rationals
  | NumVar Var -- Number Variables
  | NumInputVar Var -- Input Number Variables
  -- Operators on numbers
  | Add Number Number
  | Sub Number Number
  | Mul Number Number
  | Div Number Number
  | -- Conditionals
    IfNum Boolean Number Number
  | -- Conversion between Booleans and Numbers
    ToNum Boolean
  deriving (Eq)

instance Show Number where
  showsPrec prec expr = case expr of
    Integer n -> showsPrec prec n
    Rational n -> showsPrec prec n
    NumVar ref -> showString "$" . shows ref
    NumInputVar ref -> showString "$N" . shows ref
    Add x y -> showParen (prec > 6) $ showsPrec 6 x . showString " + " . showsPrec 7 y
    Sub x y -> showParen (prec > 6) $ showsPrec 6 x . showString " - " . showsPrec 7 y
    Mul x y -> showParen (prec > 7) $ showsPrec 7 x . showString " * " . showsPrec 8 y
    Div x y -> showParen (prec > 7) $ showsPrec 7 x . showString " / " . showsPrec 8 y
    IfNum p x y -> showParen (prec > 1) $ showString "if " . showsPrec 2 p . showString " then " . showsPrec 2 x . showString " else " . showsPrec 2 y
    ToNum x -> showString "ToNum " . showsPrec prec x

instance Num Number where
  (+) = Add
  (-) = Sub
  (*) = Mul
  abs = id

  -- law of `signum`: abs x * signum x == x
  signum = const (Integer 1)
  fromInteger = Integer

instance Semiring Number where
  plus = Add
  times = Mul
  zero = Integer 0
  one = Integer 1

instance Ring Number where
  negate = id

instance Fractional Number where
  fromRational = Rational
  (/) = Div

--------------------------------------------------------------------------------

-- | Unsigned Integers
data UInt (w :: Nat)
  = UInt Int Integer -- Integers
  | UIntVar Int Var -- Unsigned Integer Variables
  | UIntInputVar Int Var -- Input Unsigned Integer Variables
  -- Operators on numbers
  | UIntAdd (UInt w) (UInt w)
  | UIntSub (UInt w) (UInt w)
  | UIntMul (UInt w) (UInt w)
  | UIntDiv (UInt w) (UInt w)
  | -- Conditionals
    IfUInt Boolean (UInt w) (UInt w)
  | -- Conversion between Booleans and Numbers
    ToUInt Boolean
  deriving (Eq)

instance Show (UInt w) where
  showsPrec prec expr = case expr of
    UInt _ n -> showsPrec prec n
    UIntVar _ ref -> showString "$" . shows ref
    UIntInputVar width ref -> showString "$U[" . shows width . showString "]" . shows ref
    UIntAdd x y -> showParen (prec > 6) $ showsPrec 6 x . showString " + " . showsPrec 7 y
    UIntSub x y -> showParen (prec > 6) $ showsPrec 6 x . showString " - " . showsPrec 7 y
    UIntMul x y -> showParen (prec > 7) $ showsPrec 7 x . showString " * " . showsPrec 8 y
    UIntDiv x y -> showParen (prec > 7) $ showsPrec 7 x . showString " / " . showsPrec 8 y
    IfUInt p x y -> showParen (prec > 1) $ showString "if " . showsPrec 2 p . showString " then " . showsPrec 2 x . showString " else " . showsPrec 2 y
    ToUInt x -> showString "ToU " . showsPrec prec x

instance KnownNat w => Num (UInt w) where
  (+) = UIntAdd
  (-) = UIntSub
  (*) = UIntMul
  abs = id

  -- law of `signum`: abs x * signum x == x
  signum x =
    let width = natVal x
     in UInt (fromIntegral width) 1

  fromInteger = go
    where
      width = natVal (Proxy :: Proxy w)
      go :: forall width. KnownNat width => Integer -> UInt width
      go n = UInt (fromIntegral width) (fromIntegral n)

instance KnownNat w => Semiring (UInt w) where
  plus = UIntAdd
  times = UIntMul
  zero = 0
  one = 1

instance KnownNat w => Ring (UInt w) where
  negate = id

-- instance Fractional Number where
--   fromRational = Rational
--   (/) = Div

--------------------------------------------------------------------------------

-- | Booleans
data Boolean
  = Boolean Bool
  | BoolVar Var -- Boolean Variables
  | BoolInputVar Var -- Input Boolean Variables
  | NumBit Number Int
  | -- Operators on Booleans
    And Boolean Boolean
  | Or Boolean Boolean
  | Xor Boolean Boolean
  | -- Equalities
    BEq Boolean Boolean
  | Eq Number Number
  | -- Conditionals
    IfBool Boolean Boolean Boolean
  deriving (Eq)

instance Show Boolean where
  showsPrec prec expr = case expr of
    Boolean b -> showsPrec prec b
    BoolVar ref -> showString "$" . shows ref
    BoolInputVar ref -> showString "$B" . shows ref
    NumBit n i -> showsPrec prec n . showString "[" . shows i . showString "]"
    Eq x y -> showParen (prec > 5) $ showsPrec 6 x . showString " = " . showsPrec 6 y
    And x y -> showParen (prec > 3) $ showsPrec 4 x . showString " ∧ " . showsPrec 3 y
    Or x y -> showParen (prec > 2) $ showsPrec 3 x . showString " ∨ " . showsPrec 2 y
    Xor x y -> showParen (prec > 4) $ showsPrec 5 x . showString " ⊕ " . showsPrec 4 y
    BEq x y -> showParen (prec > 5) $ showsPrec 6 x . showString " = " . showsPrec 6 y
    IfBool p x y -> showParen (prec > 1) $ showString "if " . showsPrec 2 p . showString " then " . showsPrec 2 x . showString " else " . showsPrec 2 y

--------------------------------------------------------------------------------

newtype Arr t = Arr (Array Int t)
  deriving (Eq, Functor, Foldable, Traversable)

instance Show t => Show (Arr t) where
  showsPrec _prec (Arr arr) = showList (toList arr)

data ArrM t = ArrayRef ElemType Int Addr
  deriving (Eq)

--------------------------------------------------------------------------------

-- | An synonym of 'ToNum' for converting booleans to numbers
fromBool :: Boolean -> Number
fromBool = ToNum

-- | For converting numbers to booleans
toBool :: Number -> Boolean
toBool x = IfBool (x `Eq` 0) false true

-- | Smart constructor for 'True'
true :: Boolean
true = Boolean True

-- | Smart constructor for 'False'
false :: Boolean
false = Boolean False

-- | Helper function for not-`Eq`
neq :: Number -> Number -> Boolean
neq x y = IfBool (x `Eq` y) false true

-- | Helper function for not-`BEq`
nbeq :: Boolean -> Boolean -> Boolean
nbeq x y = IfBool (x `BEq` y) false true

-- | Helper function for negating a boolean expression
neg :: Boolean -> Boolean
neg x = true `Xor` x

--------------------------------------------------------------------------------

-- | Retrieve the i-th bit of a Number and return it as Boolean
--   The LSB is the 0-th bit and the MSB is the (n-1)-th bit
--      where n is the number of bits of the Number
--   You can access the MSB with (-1) because the index is modulo n
(!!!) :: Number -> Int -> Boolean
x !!! i = NumBit x i
