{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Keelung.Syntax.Kinded
  ( Number (..),
    Boolean (..),
    Unit (..),
    Arr (..),
    ArrM (..),
    fromBool,
    toBool,
    true,
    false,
    unit,
    nbeq,
    neq,
    neg,
  )
where

import Data.Array.Unboxed (Array)
import Data.Kind (Type)
import Data.Semiring (Ring (..), Semiring (..))
import Keelung.Types

--------------------------------------------------------------------------------

-- | Numbers
data Number
  = Integer Integer -- Integers
  | Rational Rational -- Rationals
  | NumberRef Var -- Reference
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
    NumberRef ref -> shows ref
    Add x y -> showParen (prec > 6) $ showsPrec 6 x . showString " + " . showsPrec 7 y
    Sub x y -> showParen (prec > 6) $ showsPrec 6 x . showString " - " . showsPrec 7 y
    Mul x y -> showParen (prec > 7) $ showsPrec 7 x . showString " * " . showsPrec 8 y
    Div x y -> showParen (prec > 7) $ showsPrec 7 x . showString " / " . showsPrec 8 y
    IfNum p x y -> showParen (prec > 1) $ showString "if " . showsPrec 2 p . showString " then " . showsPrec 2 x . showString " else " . showsPrec 2 y
    ToNum x -> showString "ToNum " . showsPrec prec x

--------------------------------------------------------------------------------

-- | Booleans
data Boolean
  = Boolean Bool
  | BooleanRef Var -- Reference
  -- Operators on Booleans
  | And Boolean Boolean
  | Or Boolean Boolean
  | Xor Boolean Boolean
  | -- Equalities
    BEq Boolean Boolean
  | Eq Number Number
  | -- Conditionals
    IfBool Boolean Boolean Boolean
  | -- Conversion between Booleans and Numbers
    ToBool Number
  deriving (Eq)

data Unit = Unit
  deriving (Eq)

newtype Arr t = Arr (Array Int t)
  deriving (Eq)

data ArrM t
  = BoolVar Var
  | BoolInputVar Var
  | NumVar Var
  | NumInputVar Var
  | ArrayRef ElemType Int Addr
  deriving (Eq)

-- UnitVal -> showString "unit"
-- ArrayVal xs -> shows xs
-- Ref ref -> shows ref

instance Show Boolean where
  showsPrec prec expr = case expr of
    Boolean b -> showsPrec prec b
    BooleanRef ref -> shows ref
    Eq x y -> showParen (prec > 5) $ showsPrec 6 x . showString " = " . showsPrec 6 y
    And x y -> showParen (prec > 3) $ showsPrec 4 x . showString " ∧ " . showsPrec 3 y
    Or x y -> showParen (prec > 2) $ showsPrec 3 x . showString " ∨ " . showsPrec 2 y
    Xor x y -> showParen (prec > 4) $ showsPrec 5 x . showString " ⊕ " . showsPrec 4 y
    BEq x y -> showParen (prec > 5) $ showsPrec 6 x . showString " = " . showsPrec 6 y
    IfBool p x y -> showParen (prec > 1) $ showString "if " . showsPrec 2 p . showString " then " . showsPrec 2 x . showString " else " . showsPrec 2 y
    ToBool x -> showString "ToBool " . showsPrec prec x

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

-- | An synonym of 'ToNum' for converting booleans to numbers
fromBool :: Boolean -> Number
fromBool = ToNum

-- | An synonym of 'ToBool' for converting numbers to booleans
toBool :: Number -> Boolean
toBool = ToBool

-- | Smart constructor for 'True'
true :: Boolean
true = Boolean True

-- | Smart constructor for 'False'
false :: Boolean
false = Boolean False

-- | Smart constructor for 'Unit'
unit :: Unit
unit = Unit

-- | Helper function for not-`Eq`
neq :: Number -> Number -> Boolean
neq x y = IfBool (x `Eq` y) false true

-- | Helper function for not-`BEq`
nbeq :: Boolean -> Boolean -> Boolean
nbeq x y = IfBool (x `BEq` y) false true

-- | Helper function for negating a boolean expression
neg :: Boolean -> Boolean
neg x = true `Xor` x
