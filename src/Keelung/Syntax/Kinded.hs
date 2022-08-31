{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Keelung.Syntax.Kinded
  ( Val (..),
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

-- | Values are indexed by 'Kind' and parameterised by some field
data Val :: Kind -> Type where
  -- Base Values
  Integer :: Integer -> Val 'Num -- Integers 
  Rational :: Rational -> Val 'Num -- Rationals 
  Boolean :: Bool -> Val 'Bool -- Booleans
  UnitVal :: Val 'Unit -- Unit
  ArrayVal :: Array Int (Val t) -> Val ('Arr t) -- Arrays
  -- Reference
  Ref :: Ref t -> Val t
  -- Operators on numbers
  Add :: Val 'Num -> Val 'Num -> Val 'Num
  Sub :: Val 'Num -> Val 'Num -> Val 'Num
  Mul :: Val 'Num -> Val 'Num -> Val 'Num
  Div :: Val 'Num -> Val 'Num -> Val 'Num
  Eq :: Val 'Num -> Val 'Num -> Val 'Bool
  -- Operators on booleans
  And :: Val 'Bool -> Val 'Bool -> Val 'Bool
  Or :: Val 'Bool -> Val 'Bool -> Val 'Bool
  Xor :: Val 'Bool -> Val 'Bool -> Val 'Bool
  BEq :: Val 'Bool -> Val 'Bool -> Val 'Bool
  -- if...then...else clause
  IfNum :: Val 'Bool -> Val 'Num -> Val 'Num -> Val 'Num
  IfBool :: Val 'Bool -> Val 'Bool -> Val 'Bool -> Val 'Bool
  -- Conversion between Booleans and Field numbers
  ToBool :: Val 'Num -> Val 'Bool
  ToNum :: Val 'Bool -> Val 'Num

-- instance Functor (Val ty) where
--   fmap f expr = case expr of
--     Integer n -> Integer n
--     Rational n -> Rational n 
--     Boolean b -> Boolean b
--     UnitVal -> UnitVal
--     ArrayVal xs -> ArrayVal (fmap (fmap f) xs)
--     Ref ref -> Ref ref
--     Add x y -> Add (fmap f x) (fmap f y)
--     Sub x y -> Sub (fmap f x) (fmap f y)
--     Mul x y -> Mul (fmap f x) (fmap f y)
--     Div x y -> Div (fmap f x) (fmap f y)
--     Eq x y -> Eq (fmap f x) (fmap f y)
--     And x y -> And (fmap f x) (fmap f y)
--     Or x y -> Or (fmap f x) (fmap f y)
--     Xor x y -> Xor (fmap f x) (fmap f y)
--     BEq x y -> BEq (fmap f x) (fmap f y)
--     IfNum p x y -> IfNum (fmap f p) (fmap f x) (fmap f y)
--     IfBool p x y -> IfBool (fmap f p) (fmap f x) (fmap f y)
--     ToBool x -> ToBool (fmap f x)
--     ToNum x -> ToNum (fmap f x)

instance Show (Val t) where
  showsPrec prec expr = case expr of
    Integer n -> showsPrec prec n
    Rational n -> showsPrec prec n 
    Boolean b -> showsPrec prec b
    UnitVal -> showString "unit"
    ArrayVal xs -> shows xs
    Ref ref -> shows ref
    Add x y -> showParen (prec > 6) $ showsPrec 6 x . showString " + " . showsPrec 7 y
    Sub x y -> showParen (prec > 6) $ showsPrec 6 x . showString " - " . showsPrec 7 y
    Mul x y -> showParen (prec > 7) $ showsPrec 7 x . showString " * " . showsPrec 8 y
    Div x y -> showParen (prec > 7) $ showsPrec 7 x . showString " / " . showsPrec 8 y
    Eq x y -> showParen (prec > 5) $ showsPrec 6 x . showString " = " . showsPrec 6 y
    And x y -> showParen (prec > 3) $ showsPrec 4 x . showString " ∧ " . showsPrec 3 y
    Or x y -> showParen (prec > 2) $ showsPrec 3 x . showString " ∨ " . showsPrec 2 y
    Xor x y -> showParen (prec > 4) $ showsPrec 5 x . showString " ⊕ " . showsPrec 4 y
    BEq x y -> showParen (prec > 5) $ showsPrec 6 x . showString " = " . showsPrec 6 y
    IfNum p x y -> showParen (prec > 1) $ showString "if " . showsPrec 2 p . showString " then " . showsPrec 2 x . showString " else " . showsPrec 2 y
    IfBool p x y -> showParen (prec > 1) $ showString "if " . showsPrec 2 p . showString " then " . showsPrec 2 x . showString " else " . showsPrec 2 y
    ToBool x -> showString "ToBool " . showsPrec prec x
    ToNum x -> showString "ToNum " . showsPrec prec x

instance Eq (Val t) where
  a == b = case (a, b) of
    (Integer x, Integer y) -> x == y
    (Rational x, Rational y) -> x == y
    (Boolean x, Boolean y) -> x == y
    (UnitVal, UnitVal) -> True
    (Ref x, Ref y) -> x == y
    (Add x y, Add z w) -> x == z && y == w
    (Sub x y, Sub z w) -> x == z && y == w
    (Mul x y, Mul z w) -> x == z && y == w
    (Div x y, Div z w) -> x == z && y == w
    (Eq x y, Eq z w) -> x == z && y == w
    (And x y, And z w) -> x == z && y == w
    (Or x y, Or z w) -> x == z && y == w
    (Xor x y, Xor z w) -> x == z && y == w
    (BEq x y, BEq z w) -> x == z && y == w
    (IfNum x y z, IfNum u v w) -> x == u && y == v && z == w
    (IfBool x y z, IfBool u v w) -> x == u && y == v && z == w
    (ToBool x, ToBool y) -> x == y
    (ToNum x, ToNum y) -> x == y
    _ -> False

instance Num (Val 'Num) where
  (+) = Add
  (-) = Sub
  (*) = Mul
  abs = id

  -- law of `signum`: abs x * signum x == x
  signum = const (Integer 1)
  fromInteger = Integer

instance Semiring (Val 'Num) where
  plus = Add
  times = Mul
  zero = Integer 0
  one = Integer 1

instance Ring (Val 'Num) where
  negate = id

instance Fractional (Val 'Num) where
  fromRational = Rational
  (/) = Div

--------------------------------------------------------------------------------

-- | An synonym of 'ToNum' for converting booleans to numbers
fromBool :: Val 'Bool -> Val 'Num
fromBool = ToNum

-- | An synonym of 'ToBool' for converting numbers to booleans
toBool :: Val 'Num -> Val 'Bool
toBool = ToBool

-- | Smart constructor for 'True'
true :: Val 'Bool
true = Boolean True

-- | Smart constructor for 'False'
false :: Val 'Bool
false = Boolean False

-- | Smart constructor for 'Unit'
unit :: Val 'Unit
unit = UnitVal

-- | Helper function for not-`Eq`
neq :: Val 'Num -> Val 'Num -> Val 'Bool
neq x y = IfBool (x `Eq` y) false true

-- | Helper function for not-`BEq`
nbeq :: Val 'Bool -> Val 'Bool -> Val 'Bool
nbeq x y = IfBool (x `BEq` y) false true

-- | Helper function for negating a boolean expression
neg :: Val 'Bool -> Val 'Bool
neg x = true `Xor` x
