{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Keelung.Syntax.Kinded
  ( Number (..),
    Boolean (..),
    UInt (..),
    Arr (..),
    ArrM (..),
    fromBool,
    toBool,
    true,
    false,
    nbeq,
    neq,
    neg,
  )
where

import Data.Array.Unboxed (Array)
import Data.Data
import Data.Foldable (toList)
import Data.Semiring (Ring (..), Semiring (..))
import GHC.TypeNats
import Keelung.Types

--------------------------------------------------------------------------------

-- | Numbers
data Number
  = Integer Integer -- Integers
  | Rational Rational -- Rationals
  | NumVar Var -- Number Variables
  | NumInputVar Var -- Input Number Variables
  | -- | PackNum [Boolean] -- Pack a list of Booleans into a Number
    -- Numeric operators on numbers
    Add Number Number
  | Sub Number Number
  | Mul Number Number
  | Div Number Number
  | -- Bitwise operators on numbers
    AndNum Number Number
  | OrNum Number Number
  | XorNum Number Number
  | RotateRNum Int Number
  | -- Conditionals
    IfNum Boolean Number Number
  | -- Conversion between Booleans and Numbers
    FromBool Boolean
  | forall w. KnownNat w => FromUInt (UInt w)

instance Eq Number where
  Integer x == Integer y = x == y
  Rational x == Rational y = x == y
  NumVar x == NumVar y = x == y
  NumInputVar x == NumInputVar y = x == y
  Add x1 x2 == Add y1 y2 = x1 == y1 && x2 == y2
  Sub x1 x2 == Sub y1 y2 = x1 == y1 && x2 == y2
  Mul x1 x2 == Mul y1 y2 = x1 == y1 && x2 == y2
  Div x1 x2 == Div y1 y2 = x1 == y1 && x2 == y2
  IfNum x1 x2 x3 == IfNum y1 y2 y3 = x1 == y1 && x2 == y2 && x3 == y3
  FromBool x == FromBool y = x == y
  FromUInt x == FromUInt y = case sameNat x y of
    Just Refl -> x == y
    Nothing -> False
  _ == _ = False

instance Show Number where
  showsPrec prec expr = case expr of
    Integer n -> showsPrec prec n
    Rational n -> showsPrec prec n
    NumVar ref -> showString "$" . shows ref
    NumInputVar ref -> showString "$N" . shows ref
    -- PackNum bs -> showString "Pack(" . shows bs . showString ")"
    Add x y -> showParen (prec > 6) $ showsPrec 6 x . showString " + " . showsPrec 7 y
    Sub x y -> showParen (prec > 6) $ showsPrec 6 x . showString " - " . showsPrec 7 y
    Mul x y -> showParen (prec > 7) $ showsPrec 7 x . showString " * " . showsPrec 8 y
    Div x y -> showParen (prec > 7) $ showsPrec 7 x . showString " / " . showsPrec 8 y
    AndNum x y -> showParen (prec > 5) $ showsPrec 5 x . showString " ∧ " . showsPrec 6 y
    OrNum x y -> showParen (prec > 4) $ showsPrec 4 x . showString " ∨ " . showsPrec 5 y
    XorNum x y -> showParen (prec > 3) $ showsPrec 3 x . showString " ⊕ " . showsPrec 4 y
    RotateRNum n x -> showParen (prec > 8) $ showString "ROTATE " . showsPrec 9 n . showString " " . showsPrec 9 x
    IfNum p x y -> showParen (prec > 1) $ showString "if " . showsPrec 2 p . showString " then " . showsPrec 2 x . showString " else " . showsPrec 2 y
    FromBool x -> showString "FromBool " . showsPrec prec x
    FromUInt x -> showString "FromUInt " . showsPrec prec x

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
  -- Numeric operators on unsigned integers
  | UIntAdd (UInt w) (UInt w)
  | UIntSub (UInt w) (UInt w)
  | UIntMul (UInt w) (UInt w)
  | UIntDiv (UInt w) (UInt w)
  | -- Bitwise operators on unsigned integers
    AndUInt (UInt w) (UInt w)
  | OrUInt (UInt w) (UInt w)
  | XorUInt (UInt w) (UInt w)
  | RotateRUInt Int (UInt w)
  | -- Conditionals
    IfUInt Boolean (UInt w) (UInt w)
  | -- Conversion between Booleans and unsigned integers
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
    AndUInt x y -> showParen (prec > 5) $ showsPrec 5 x . showString " ∧ " . showsPrec 6 y
    OrUInt x y -> showParen (prec > 4) $ showsPrec 4 x . showString " ∨ " . showsPrec 5 y
    XorUInt x y -> showParen (prec > 3) $ showsPrec 3 x . showString " ⊕ " . showsPrec 4 y
    RotateRUInt n x -> showParen (prec > 8) $ showString "ROTATE " . showsPrec 9 n . showString " " . showsPrec 9 x
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
  | forall w. KnownNat w => UIntBit (UInt w) Int
  | -- Operators on Booleans
    And Boolean Boolean
  | Or Boolean Boolean
  | Xor Boolean Boolean
  | -- Equalities
    BEq Boolean Boolean
  | Eq Number Number
  | forall w. KnownNat w => UEq (UInt w) (UInt w)
  | -- Conditionals
    IfBool Boolean Boolean Boolean

instance Eq Boolean where
  Boolean x == Boolean y = x == y
  BoolVar x == BoolVar y = x == y
  BoolInputVar x == BoolInputVar y = x == y
  NumBit x1 x2 == NumBit y1 y2 = x1 == y1 && x2 == y2
  And x1 x2 == And y1 y2 = x1 == y1 && x2 == y2
  Or x1 x2 == Or y1 y2 = x1 == y1 && x2 == y2
  Xor x1 x2 == Xor y1 y2 = x1 == y1 && x2 == y2
  BEq x1 x2 == BEq y1 y2 = x1 == y1 && x2 == y2
  Eq x1 x2 == Eq y1 y2 = x1 == y1 && x2 == y2
  UEq x1 x2 == UEq y1 y2 = case sameNat x1 x2 of
    Just Refl -> case sameNat y1 y2 of
      Just Refl -> (x1 == x2) == (y1 == y2)
      Nothing -> False
    Nothing -> False
  IfBool x1 x2 x3 == IfBool y1 y2 y3 = x1 == y1 && x2 == y2 && x3 == y3
  _ == _ = False

instance Show Boolean where
  showsPrec prec expr = case expr of
    Boolean b -> showsPrec prec b
    BoolVar ref -> showString "$" . shows ref
    BoolInputVar ref -> showString "$B" . shows ref
    NumBit n i -> showsPrec prec n . showString "[" . shows i . showString "]"
    UIntBit n i -> showsPrec prec n . showString "[" . shows i . showString "]"
    Eq x y -> showParen (prec > 5) $ showsPrec 6 x . showString " = " . showsPrec 6 y
    And x y -> showParen (prec > 3) $ showsPrec 4 x . showString " ∧ " . showsPrec 3 y
    Or x y -> showParen (prec > 2) $ showsPrec 3 x . showString " ∨ " . showsPrec 2 y
    Xor x y -> showParen (prec > 4) $ showsPrec 5 x . showString " ⊕ " . showsPrec 4 y
    BEq x y -> showParen (prec > 5) $ showsPrec 6 x . showString " = " . showsPrec 6 y
    UEq x y -> showParen (prec > 5) $ showsPrec 6 x . showString " = " . showsPrec 6 y
    IfBool p x y -> showParen (prec > 1) $ showString "if " . showsPrec 2 p . showString " then " . showsPrec 2 x . showString " else " . showsPrec 2 y

--------------------------------------------------------------------------------

newtype Arr t = Arr (Array Int t)
  deriving (Eq, Functor, Foldable, Traversable)

instance Show t => Show (Arr t) where
  showsPrec _prec (Arr arr) = showList (toList arr)

data ArrM t = ArrayRef ElemType Int Addr
  deriving (Eq)

--------------------------------------------------------------------------------

-- | An synonym of 'FromBool' for converting booleans to numbers
fromBool :: Boolean -> Number
fromBool = FromBool

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
