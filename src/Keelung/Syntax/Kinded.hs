{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Keelung.Syntax.Kinded
  ( Number (..),
    Boolean (..),
    UInt (..),
    widthOf,
    Arr (..),
    ArrM (..),
    Cmp (..),
    fromBool,
    toBool,
    true,
    false,
    complement,
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
  | VarN Var -- Number Variables
  | InputVarN Var -- Input Number Variables
  | -- | PackNum [Boolean] -- Pack a list of Booleans into a Number
    -- Numeric operators on numbers
    Add Number Number
  | Sub Number Number
  | Mul Number Number
  | Div Number Number
  | -- Bitwise operators on numbers
    --   AndN Number Number

    -- | OrN Number Number
    -- | XorN Number Number
    -- RoRN I nt Number
    -- Conditionals
    IfN Boolean Number Number
  | -- Conversion between Booleans and Numbers
    FromBool Boolean
  | forall w. KnownNat w => FromUInt (UInt w)

instance Eq Number where
  Integer x == Integer y = x == y
  Rational x == Rational y = x == y
  VarN x == VarN y = x == y
  InputVarN x == InputVarN y = x == y
  Add x1 x2 == Add y1 y2 = x1 == y1 && x2 == y2
  Sub x1 x2 == Sub y1 y2 = x1 == y1 && x2 == y2
  Mul x1 x2 == Mul y1 y2 = x1 == y1 && x2 == y2
  Div x1 x2 == Div y1 y2 = x1 == y1 && x2 == y2
  IfN x1 x2 x3 == IfN y1 y2 y3 = x1 == y1 && x2 == y2 && x3 == y3
  FromBool x == FromBool y = x == y
  FromUInt x == FromUInt y = case sameNat x y of
    Just Refl -> x == y
    Nothing -> False
  _ == _ = False

instance Show Number where
  showsPrec prec expr = case expr of
    Integer n -> showsPrec prec n
    Rational n -> showsPrec prec n
    VarN ref -> showString "$" . shows ref
    InputVarN ref -> showString "$N" . shows ref
    -- PackNum bs -> showString "Pack(" . shows bs . showString ")"
    Add x y -> showParen (prec > 6) $ showsPrec 6 x . showString " + " . showsPrec 7 y
    Sub x y -> showParen (prec > 6) $ showsPrec 6 x . showString " - " . showsPrec 7 y
    Mul x y -> showParen (prec > 7) $ showsPrec 7 x . showString " * " . showsPrec 8 y
    Div x y -> showParen (prec > 7) $ showsPrec 7 x . showString " / " . showsPrec 8 y
    -- AndN x y -> showParen (prec > 5) $ showsPrec 5 x . showString " ∧ " . showsPrec 6 y
    -- OrN x y -> showParen (prec > 4) $ showsPrec 4 x . showString " ∨ " . showsPrec 5 y
    -- XorN x y -> showParen (prec > 3) $ showsPrec 3 x . showString " ⊕ " . showsPrec 4 y
    -- RoRN n x -> showParen (prec > 8) $ showString "ROTATE " . showsPrec 9 n . showString " " . showsPrec 9 x
    IfN p x y -> showParen (prec > 1) $ showString "if " . showsPrec 2 p . showString " then " . showsPrec 2 x . showString " else " . showsPrec 2 y
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
  = UInt Integer -- Integers
  | VarU Var -- Unsigned Integer Variables
  | InputVarU Var -- Input Unsigned Integer Variables
  -- Numeric operators on unsigned integers
  | AddU (UInt w) (UInt w)
  | SubU (UInt w) (UInt w)
  | MulU (UInt w) (UInt w)
  | -- Bitwise operators on unsigned integers
    AndU (UInt w) (UInt w)
  | OrU (UInt w) (UInt w)
  | XorU (UInt w) (UInt w)
  | NotU (UInt w)
  | RoLU Int (UInt w)
  | -- Conditionals
    IfU Boolean (UInt w) (UInt w)
  | -- Conversion between Booleans and unsigned integers
    ToUInt Boolean
  deriving (Eq)

instance KnownNat w => Show (UInt w) where
  showsPrec prec expr = case expr of
    UInt n -> showsPrec prec n
    VarU var -> showString "$" . shows var
    InputVarU var -> showString "$U[" . shows (widthOf expr) . showString "]" . shows var
    AddU x y -> showParen (prec > 6) $ showsPrec 6 x . showString " + " . showsPrec 7 y
    SubU x y -> showParen (prec > 6) $ showsPrec 6 x . showString " - " . showsPrec 7 y
    MulU x y -> showParen (prec > 7) $ showsPrec 7 x . showString " * " . showsPrec 8 y
    AndU x y -> showParen (prec > 5) $ showsPrec 5 x . showString " ∧ " . showsPrec 6 y
    OrU x y -> showParen (prec > 4) $ showsPrec 4 x . showString " ∨ " . showsPrec 5 y
    XorU x y -> showParen (prec > 3) $ showsPrec 3 x . showString " ⊕ " . showsPrec 4 y
    NotU x -> showParen (prec > 8) $ showString "¬ " . showsPrec 9 x
    RoLU n x -> showParen (prec > 8) $ showString "RoL " . showsPrec 9 n . showString " " . showsPrec 9 x
    IfU p x y -> showParen (prec > 1) $ showString "if " . showsPrec 2 p . showString " then " . showsPrec 2 x . showString " else " . showsPrec 2 y
    ToUInt x -> showString "ToU " . showsPrec prec x

instance KnownNat w => Num (UInt w) where
  (+) = AddU
  (-) = SubU
  (*) = MulU
  abs = id

  -- law of `signum`: abs x * signum x == x
  signum _ = UInt 1

  fromInteger = go
    where
      -- width = natVal (Proxy :: Proxy w)
      go :: forall width. KnownNat width => Integer -> UInt width
      go n = UInt (fromIntegral n)

instance KnownNat w => Semiring (UInt w) where
  plus = AddU
  times = MulU
  zero = 0
  one = 1

instance KnownNat w => Ring (UInt w) where
  negate = id

-- instance Fractional Number where
--   fromRational = Rational
--   (/) = Div

--------------------------------------------------------------------------------

-- | Typeclass for deriving the bit width of an expression
class HasWidth a where
  widthOf :: a -> Int

instance KnownNat w => HasWidth (UInt w) where
  widthOf _ = fromIntegral $ natVal (Proxy :: Proxy w)

--------------------------------------------------------------------------------

-- | Booleans
data Boolean
  = Boolean Bool
  | VarB Var -- Boolean Variables
  | InputVarB Var -- Input Boolean Variables
  | NumBit Number Int
  | forall w. KnownNat w => UIntBit (UInt w) Int
  | -- Operators on Booleans
    And Boolean Boolean
  | Or Boolean Boolean
  | Xor Boolean Boolean
  | -- Equalities
    EqB Boolean Boolean
  | EqN Number Number
  | forall w. KnownNat w => EqU (UInt w) (UInt w)
  | -- Conditionals
    IfB Boolean Boolean Boolean

instance Eq Boolean where
  Boolean x == Boolean y = x == y
  VarB x == VarB y = x == y
  InputVarB x == InputVarB y = x == y
  NumBit x1 x2 == NumBit y1 y2 = x1 == y1 && x2 == y2
  And x1 x2 == And y1 y2 = x1 == y1 && x2 == y2
  Or x1 x2 == Or y1 y2 = x1 == y1 && x2 == y2
  Xor x1 x2 == Xor y1 y2 = x1 == y1 && x2 == y2
  EqB x1 x2 == EqB y1 y2 = x1 == y1 && x2 == y2
  EqN x1 x2 == EqN y1 y2 = x1 == y1 && x2 == y2
  EqU x1 x2 == EqU y1 y2 = case sameNat x1 x2 of
    Just Refl -> case sameNat y1 y2 of
      Just Refl -> (x1 == x2) == (y1 == y2)
      Nothing -> False
    Nothing -> False
  IfB x1 x2 x3 == IfB y1 y2 y3 = x1 == y1 && x2 == y2 && x3 == y3
  _ == _ = False

instance Show Boolean where
  showsPrec prec expr = case expr of
    Boolean b -> showsPrec prec b
    VarB ref -> showString "$" . shows ref
    InputVarB ref -> showString "$B" . shows ref
    NumBit n i -> showsPrec prec n . showString "[" . shows i . showString "]"
    UIntBit n i -> showsPrec prec n . showString "[" . shows i . showString "]"
    EqN x y -> showParen (prec > 5) $ showsPrec 6 x . showString " = " . showsPrec 6 y
    And x y -> showParen (prec > 3) $ showsPrec 4 x . showString " ∧ " . showsPrec 3 y
    Or x y -> showParen (prec > 2) $ showsPrec 3 x . showString " ∨ " . showsPrec 2 y
    Xor x y -> showParen (prec > 4) $ showsPrec 5 x . showString " ⊕ " . showsPrec 4 y
    EqB x y -> showParen (prec > 5) $ showsPrec 6 x . showString " = " . showsPrec 6 y
    EqU x y -> showParen (prec > 5) $ showsPrec 6 x . showString " = " . showsPrec 6 y
    IfB p x y -> showParen (prec > 1) $ showString "if " . showsPrec 2 p . showString " then " . showsPrec 2 x . showString " else " . showsPrec 2 y

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
toBool x = IfB (x `EqN` 0) false true

-- | Smart constructor for 'True'
true :: Boolean
true = Boolean True

-- | Smart constructor for 'False'
false :: Boolean
false = Boolean False

-- | Helper function for not-`Eq`
-- neq :: Number -> Number -> Boolean
-- neq x y = IfB (x `Eq` y) false true

-- -- | Helper function for not-`EqB`
-- nbeq :: Boolean -> Boolean -> Boolean
-- nbeq x y = IfB (x `EqB` y) false true

-- | Helper function for negating a boolean expression
complement :: Boolean -> Boolean
complement x = true `Xor` x

class Cmp a where
  eq :: a -> a -> Boolean
  neq :: a -> a -> Boolean

instance Cmp Boolean where
  eq = EqB
  neq x y = IfB (x `eq` y) false true

instance Cmp Number where
  eq = EqN
  neq x y = IfB (x `eq` y) false true

instance KnownNat w => Cmp (UInt w) where
  eq = EqU
  neq x y = IfB (x `eq` y) false true