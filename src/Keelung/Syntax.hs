{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Keelung.Syntax
  ( Expr (..),
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

import Data.Field.Galois (GaloisField (..))
import Data.Kind (Type)
import Data.Semiring (Ring (..), Semiring (..))
import Keelung.Types

--------------------------------------------------------------------------------

-- | Expressions are indexed by 'Kind' and parameterised by some field
data Expr :: Kind -> Type -> Type where
  -- Value & Reference
  Number :: n -> Expr 'Num n -- Field numbers
  Boolean :: Bool -> Expr 'Bool n -- Booleans
  UnitVal :: Expr 'Unit n -- Unit
  -- Reference
  Ref :: Ref t -> Expr t n
  -- Operators on numbers
  Add :: Expr 'Num n -> Expr 'Num n -> Expr 'Num n
  Sub :: Expr 'Num n -> Expr 'Num n -> Expr 'Num n
  Mul :: Expr 'Num n -> Expr 'Num n -> Expr 'Num n
  Div :: Expr 'Num n -> Expr 'Num n -> Expr 'Num n
  Eq :: Expr 'Num n -> Expr 'Num n -> Expr 'Bool n
  -- Operators on booleans
  And :: Expr 'Bool n -> Expr 'Bool n -> Expr 'Bool n
  Or :: Expr 'Bool n -> Expr 'Bool n -> Expr 'Bool n
  Xor :: Expr 'Bool n -> Expr 'Bool n -> Expr 'Bool n
  BEq :: Expr 'Bool n -> Expr 'Bool n -> Expr 'Bool n
  -- if...then...else clause
  IfNum :: Expr 'Bool n -> Expr 'Num n -> Expr 'Num n -> Expr 'Num n
  IfBool :: Expr 'Bool n -> Expr 'Bool n -> Expr 'Bool n -> Expr 'Bool n
  -- Conversion between Booleans and Field numbers
  ToBool :: Expr 'Num n -> Expr 'Bool n
  ToNum :: Expr 'Bool n -> Expr 'Num n

instance Functor (Expr ty) where
  fmap f expr = case expr of
    Number n -> Number (f n)
    Boolean b -> Boolean b
    UnitVal -> UnitVal
    Ref ref -> Ref ref
    Add x y -> Add (fmap f x) (fmap f y)
    Sub x y -> Sub (fmap f x) (fmap f y)
    Mul x y -> Mul (fmap f x) (fmap f y)
    Div x y -> Div (fmap f x) (fmap f y)
    Eq x y -> Eq (fmap f x) (fmap f y)
    And x y -> And (fmap f x) (fmap f y)
    Or x y -> Or (fmap f x) (fmap f y)
    Xor x y -> Xor (fmap f x) (fmap f y)
    BEq x y -> BEq (fmap f x) (fmap f y)
    IfNum p x y -> IfNum (fmap f p) (fmap f x) (fmap f y)
    IfBool p x y -> IfBool (fmap f p) (fmap f x) (fmap f y)
    ToBool x -> ToBool (fmap f x)
    ToNum x -> ToNum (fmap f x)

instance Show n => Show (Expr ty n) where
  showsPrec prec expr = case expr of
    Number n -> showsPrec prec n
    Boolean b -> showsPrec prec b
    UnitVal -> showString "unit"
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

instance Eq n => Eq (Expr ty n) where
  a == b = case (a, b) of
    (Number x, Number y) -> x == y
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

instance GaloisField n => Num (Expr 'Num n) where
  (+) = Add
  (-) = Sub
  (*) = Mul
  abs = id

  -- law of `signum`: abs x * signum x == x
  signum = const (Number 1)
  fromInteger = Number . fromNatural . fromInteger

instance GaloisField n => Semiring (Expr 'Num n) where
  plus = Add
  times = Mul
  zero = Number 0
  one = Number 1

instance GaloisField n => Ring (Expr 'Num n) where
  negate = id

instance GaloisField n => Fractional (Expr 'Num n) where
  fromRational = Number . fromRational
  (/) = Div

--------------------------------------------------------------------------------

-- | An synonym of 'ToNum' for converting booleans to numbers
fromBool :: GaloisField n => Expr 'Bool n -> Expr 'Num n
fromBool = ToNum

-- | An synonym of 'ToBool' for converting numbers to booleans
toBool :: GaloisField n => Expr 'Num n -> Expr 'Bool n
toBool = ToBool

-- | Smart constructor for 'True'
true :: Expr 'Bool n
true = Boolean True

-- | Smart constructor for 'False'
false :: Expr 'Bool n
false = Boolean False

-- | Smart constructor for 'Unit'
unit :: Expr 'Unit n
unit = UnitVal

-- | Helper function for not-`Eq`
neq :: Expr 'Num n -> Expr 'Num n -> Expr 'Bool n
neq x y = IfBool (x `Eq` y) false true

-- | Helper function for not-`BEq`
nbeq :: Expr 'Bool n -> Expr 'Bool n -> Expr 'Bool n
nbeq x y = IfBool (x `BEq` y) false true

-- | Helper function for negating a boolean expression
neg :: Expr 'Bool n -> Expr 'Bool n
neg x = true `Xor` x
