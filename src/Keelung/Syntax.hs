{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Syntax of the Keelung language
module Keelung.Syntax
  ( Field (..),
    Boolean (..),
    UInt (..),
    (.*.),
    HasWidth (..),
    EQ (..),
    gt,
    gte,
    lt,
    lte,
    true,
    false,
    setBit,
    modInv,
    pow,
    aesMul,
    Var,
    Width,
  )
where

import Data.Data
import GHC.TypeNats

--------------------------------------------------------------------------------

-- | Field elements.
--   The choice of the underlying field is left to be decided during the compilation.
data Field
  = -- | Integral values
    Integer Integer
  | -- | Rational values
    Rational Rational
  | -- | Field element variables
    VarF Var
  | -- | Field element public input variables
    VarFI Var
  | -- | Field element private input variables
    VarFP Var
  | -- | Addition
    Add Field Field
  | -- | Subtraction
    Sub Field Field
  | -- | Multiplication
    Mul Field Field
  | -- | Exponentiation
    Exp Field Integer
  | -- | Division (without remainders)
    Div Field Field
  | -- | Conditional that returns a Field element
    IfF Boolean Field Field
  | -- | Conversion from Booleans to Field elements
    BtoF Boolean
  deriving (Ord)

instance Eq Field where
  Integer x == Integer y = x == y
  Rational x == Rational y = x == y
  VarF x == VarF y = x == y
  VarFI x == VarFI y = x == y
  Add x1 x2 == Add y1 y2 = x1 == y1 && x2 == y2
  Sub x1 x2 == Sub y1 y2 = x1 == y1 && x2 == y2
  Mul x1 x2 == Mul y1 y2 = x1 == y1 && x2 == y2
  Div x1 x2 == Div y1 y2 = x1 == y1 && x2 == y2
  IfF x1 x2 x3 == IfF y1 y2 y3 = x1 == y1 && x2 == y2 && x3 == y3
  BtoF x == BtoF y = x == y
  _ == _ = False

instance Show Field where
  showsPrec prec expr = case expr of
    Integer n -> showsPrec prec n
    Rational n -> showsPrec prec n
    VarF ref -> showString "$F" . shows ref
    VarFI ref -> showString "$FI" . shows ref
    VarFP ref -> showString "$FP" . shows ref
    Add x y -> showParen (prec > 6) $ showsPrec 6 x . showString " + " . showsPrec 7 y
    Sub x y -> showParen (prec > 6) $ showsPrec 6 x . showString " - " . showsPrec 7 y
    Mul x y -> showParen (prec > 7) $ showsPrec 7 x . showString " * " . showsPrec 8 y
    Exp x y -> showParen (prec > 9) $ showsPrec 9 x . showString " ^ " . showsPrec 10 y
    Div x y -> showParen (prec > 7) $ showsPrec 7 x . showString " / " . showsPrec 8 y
    IfF p x y -> showParen (prec > 1) $ showString "if " . showsPrec 2 p . showString " then " . showsPrec 2 x . showString " else " . showsPrec 2 y
    BtoF x -> showString "B→F" . showsPrec prec x

instance Num Field where
  (+) = Add
  (-) = Sub
  (*) = Mul
  abs = id

  -- law of `signum`: abs x * signum x == x
  signum = const (Integer 1)
  fromInteger = Integer

instance Fractional Field where
  fromRational = Rational
  (/) = Div

--------------------------------------------------------------------------------

-- | Unsigned Integers.
--   The bit width is annotated by a type-level natural that is known at compile time.
data UInt (w :: Nat)
  = -- | Unsigned integers values
    UInt Integer
  | -- | Unsigned integer variables
    VarU Var
  | -- | Unsigned integer public input variables
    VarUI Var
  | -- | Unsigned integer private input variables
    VarUP Var
  | -- | Addition
    AddU (UInt w) (UInt w)
  | -- | Subtraction
    SubU (UInt w) (UInt w)
  | -- | Multiplication
    MulU (UInt w) (UInt w)
  | -- | Hardcoded GF(256) Multiplication for AES
    AESMulU (UInt 8) (UInt 8)
  | -- | Carry-less Multiplication
    CLMulU (UInt w) (UInt w)
  | -- | Modular multiplicatie inverse
    MMIU (UInt w) Integer
  | -- | Bitwise conjunction
    AndU (UInt w) (UInt w)
  | -- | Bitwise disjunction
    OrU (UInt w) (UInt w)
  | -- | Bitwise exclusive disjunction
    XorU (UInt w) (UInt w)
  | -- | Bitwise complement
    NotU (UInt w)
  | -- | Rotate left
    RoLU Width Int (UInt w)
  | -- | Shift left
    ShLU Width Int (UInt w)
  | -- | Bit set and return the result
    SetU (UInt w) Int Boolean
  | -- | Conditional that returns an unsigned integer
    IfU Boolean (UInt w) (UInt w)
  | -- | Conversion from Booleans to Unsigned integers
    BtoU Boolean
  deriving (Eq, Ord)

instance KnownNat w => Show (UInt w) where
  showsPrec prec expr = case expr of
    UInt n -> showsPrec prec n
    VarU var -> showString "$U" . showString (toSubscript width) . shows var
    VarUI var -> showString "$UI" . showString (toSubscript width) . shows var
    VarUP var -> showString "$UP" . showString (toSubscript width) . shows var
    AddU x y -> showParen (prec > 6) $ showsPrec 6 x . showString " + " . showsPrec 7 y
    SubU x y -> showParen (prec > 6) $ showsPrec 6 x . showString " - " . showsPrec 7 y
    MulU x y -> showParen (prec > 7) $ showsPrec 7 x . showString " * " . showsPrec 8 y
    AESMulU x y -> showParen (prec > 7) $ showsPrec 7 x . showString " AES* " . showsPrec 8 y
    CLMulU x y -> showParen (prec > 7) $ showsPrec 7 x . showString " .*. " . showsPrec 8 y
    MMIU x p -> showParen (prec > 8) $ showsPrec 9 x . showString "⁻¹ (mod " . shows p . showString ")"
    AndU x y -> showParen (prec > 5) $ showsPrec 5 x . showString " ∧ " . showsPrec 6 y
    OrU x y -> showParen (prec > 4) $ showsPrec 4 x . showString " ∨ " . showsPrec 5 y
    XorU x y -> showParen (prec > 3) $ showsPrec 3 x . showString " ⊕ " . showsPrec 4 y
    NotU x -> showParen (prec > 8) $ showString "¬ " . showsPrec 9 x
    RoLU _ n x -> showParen (prec > 8) $ showString "RoL " . showsPrec 9 n . showString " " . showsPrec 9 x
    ShLU _ n x -> showParen (prec > 8) $ showString "ShL " . showsPrec 9 n . showString " " . showsPrec 9 x
    SetU x i b -> showParen (prec > 8) $ showsPrec 9 x . showString "[" . showsPrec 9 i . showString "] := " . showsPrec 9 b
    IfU p x y -> showParen (prec > 1) $ showString "if " . showsPrec 2 p . showString " then " . showsPrec 2 x . showString " else " . showsPrec 2 y
    BtoU x -> showString "B→U " . showsPrec prec x
    where
      width :: Width
      width = widthOf expr

      toSubscript :: Int -> String
      toSubscript = map go . show
        where
          go c = case c of
            '0' -> '₀'
            '1' -> '₁'
            '2' -> '₂'
            '3' -> '₃'
            '4' -> '₄'
            '5' -> '₅'
            '6' -> '₆'
            '7' -> '₇'
            '8' -> '₈'
            '9' -> '₉'
            _ -> c

instance KnownNat w => Num (UInt w) where
  (+) = AddU
  (-) = SubU
  (*) = MulU
  abs = id

  -- law of `signum`: abs x * signum x == x
  signum _ = UInt 1

  fromInteger n = UInt (fromIntegral n)

-- | Carry-less multiplication
(.*.) :: KnownNat w => UInt w -> UInt w -> UInt w
(.*.) = CLMulU

infixl 8 .*.

instance KnownNat w => Enum (UInt w) where
  toEnum = UInt . toInteger
  fromEnum = error "[ panic ] Enum.fromEnum: undefined for UInt"

instance KnownNat w => Real (UInt w) where
  toRational = error "[ panic ] Real.toRational: undefined for UInt"

--------------------------------------------------------------------------------

-- | Typeclass for deriving the bit width of an expression
class HasWidth a where
  -- | Derive the bit width of an expression
  widthOf :: a -> Int

instance KnownNat w => HasWidth (UInt w) where
  widthOf _ = fromIntegral $ natVal (Proxy :: Proxy w)

--------------------------------------------------------------------------------

-- | Booleans
data Boolean
  = -- | Boolean values
    Boolean Bool
  | -- | Boolean variables
    VarB Var
  | -- | Boolean public input variables
    VarBI Var
  | -- | Boolean private input variables
    VarBP Var
  | -- | Conjunction
    And Boolean Boolean
  | -- | Disjunction
    Or Boolean Boolean
  | -- | Exclusive disjunction
    Xor Boolean Boolean
  | -- | Complement
    Not Boolean
  | -- | Equality on Booleans
    EqB Boolean Boolean
  | -- | Equality on Field elements
    EqF Field Field
  | -- | Equality on Unsigned integers
    forall w. KnownNat w => EqU (UInt w) (UInt w)
  | -- | GTE on Unsigned integers
    forall w. KnownNat w => GTEU (UInt w) (UInt w)
  | -- | GT on Unsigned integers
    forall w. KnownNat w => GTU (UInt w) (UInt w)
  | -- | LTE on Unsigned integers
    forall w. KnownNat w => LTEU (UInt w) (UInt w)
  | -- | LT on Unsigned integers
    forall w. KnownNat w => LTU (UInt w) (UInt w)
  | -- | Conditional that returns a Boolean
    IfB Boolean Boolean Boolean
  | -- | Bit test on Unsigned integers
    forall w. KnownNat w => BitU (UInt w) Int

-- Manually implement Ord instance for Boolean
instance Ord Boolean where
  compare (Boolean x) (Boolean y) = compare x y
  compare (VarB x) (VarB y) = compare x y
  compare (VarBI x) (VarBI y) = compare x y
  compare (And x1 x2) (And y1 y2) = compare (x1, x2) (y1, y2)
  compare (Or x1 x2) (Or y1 y2) = compare (x1, x2) (y1, y2)
  compare (Xor x1 x2) (Xor y1 y2) = compare (x1, x2) (y1, y2)
  compare (EqB x1 x2) (EqB y1 y2) = compare (x1, x2) (y1, y2)
  compare (EqF x1 x2) (EqF y1 y2) = compare (x1, x2) (y1, y2)
  compare (EqU x1 x2) (EqU y1 y2) = case sameNat x1 y1 of
    Just Refl -> case sameNat x1 y2 of
      Just Refl -> compare (x1, x2) (y1, y2)
      Nothing -> error "[ panic ] Boolean.EqU: cannot compare UInts of different widths"
    Nothing -> error "[ panic ] Boolean.EqU: cannot compare UInts of different widths"
  compare (GTEU x1 x2) (GTEU y1 y2) =
    case sameNat x1 y1 of
      Just Refl -> case sameNat x2 y2 of
        Just Refl -> compare (x1, x2) (y1, y2)
        Nothing -> error "[ panic ] Boolean. : cannot compare UInts of different widths"
      Nothing -> error "[ panic ] Boolean. : cannot compare UInts of different widths"
  compare (GTU x1 x2) (GTU y1 y2) =
    case sameNat x1 y1 of
      Just Refl -> case sameNat x2 y2 of
        Just Refl -> compare (x1, x2) (y1, y2)
        Nothing -> error "[ panic ] Boolean.GTU : cannot compare UInts of different widths"
      Nothing -> error "[ panic ] Boolean.GTU : cannot compare UInts of different widths"
  compare (LTEU x1 x2) (LTEU y1 y2) =
    case sameNat x1 y1 of
      Just Refl -> case sameNat x2 y2 of
        Just Refl -> compare (x1, x2) (y1, y2)
        Nothing -> error "[ panic ] Boolean.LTEU : cannot compare UInts of different widths"
      Nothing -> error "[ panic ] Boolean.LTEU : cannot compare UInts of different widths"
  compare (LTU x1 x2) (LTU y1 y2) =
    case sameNat x1 y1 of
      Just Refl -> case sameNat x2 y2 of
        Just Refl -> compare (x1, x2) (y1, y2)
        Nothing -> error "[ panic ] Boolean.LTU : cannot compare UInts of different widths"
      Nothing -> error "[ panic ] Boolean.LTU : cannot compare UInts of different widths"
  compare (IfB x1 x2 x3) (IfB y1 y2 y3) = compare (x1, x2, x3) (y1, y2, y3)
  compare (BitU x1 x2) (BitU y1 y2) = case sameNat x1 y1 of
    Just Refl -> compare x2 y2
    Nothing -> error "[ panic ] Boolean.BitU: cannot compare UInts of different widths"
  compare x y = compare (tag x) (tag y)
    where
      tag :: Boolean -> Int
      tag (Boolean _) = 0
      tag (VarB _) = 1
      tag (VarBI _) = 2
      tag (VarBP _) = 3
      tag (And _ _) = 4
      tag (Or _ _) = 5
      tag (Xor _ _) = 6
      tag (Not _) = 7
      tag (EqB _ _) = 8
      tag (EqF _ _) = 9
      tag (EqU _ _) = 10
      tag (GTEU _ _) = 11
      tag (GTU _ _) = 12
      tag (LTEU _ _) = 13
      tag (LTU _ _) = 14
      tag (IfB {}) = 15
      tag (BitU _ _) = 16

instance Eq Boolean where
  Boolean x == Boolean y = x == y
  VarB x == VarB y = x == y
  VarBI x == VarBI y = x == y
  And x1 x2 == And y1 y2 = x1 == y1 && x2 == y2
  Or x1 x2 == Or y1 y2 = x1 == y1 && x2 == y2
  Xor x1 x2 == Xor y1 y2 = x1 == y1 && x2 == y2
  EqB x1 x2 == EqB y1 y2 = x1 == y1 && x2 == y2
  EqF x1 x2 == EqF y1 y2 = x1 == y1 && x2 == y2
  EqU x1 x2 == EqU y1 y2 = case sameNat x1 x2 of
    Just Refl -> case sameNat y1 y2 of
      Just Refl -> (x1 == x2) == (y1 == y2)
      Nothing -> False
    Nothing -> False
  IfB x1 x2 x3 == IfB y1 y2 y3 = x1 == y1 && x2 == y2 && x3 == y3
  BitU x1 x2 == BitU y1 y2 = case sameNat x1 y1 of
    Just Refl -> x2 == y2
    Nothing -> False
  _ == _ = False

instance Show Boolean where
  showsPrec prec expr = case expr of
    Boolean b -> showsPrec prec b
    VarB ref -> showString "$B" . shows ref
    VarBI ref -> showString "$BI" . shows ref
    VarBP ref -> showString "$BP" . shows ref
    BitU n i -> showsPrec prec n . showString "[" . shows i . showString "]"
    EqF x y -> showParen (prec > 5) $ showsPrec 6 x . showString " = " . showsPrec 6 y
    And x y -> showParen (prec > 3) $ showsPrec 4 x . showString " ∧ " . showsPrec 3 y
    Or x y -> showParen (prec > 2) $ showsPrec 3 x . showString " ∨ " . showsPrec 2 y
    Xor x y -> showParen (prec > 4) $ showsPrec 5 x . showString " ⊕ " . showsPrec 4 y
    Not x -> showParen (prec > 8) $ showString "¬ " . showsPrec 9 x
    EqB x y -> showParen (prec > 5) $ showsPrec 6 x . showString " = " . showsPrec 6 y
    EqU x y -> showParen (prec > 5) $ showsPrec 6 x . showString " = " . showsPrec 6 y
    GTEU x y -> showParen (prec > 5) $ showsPrec 6 x . showString " ≥ " . showsPrec 6 y
    GTU x y -> showParen (prec > 5) $ showsPrec 6 x . showString " > " . showsPrec 6 y
    LTEU x y -> showParen (prec > 5) $ showsPrec 6 x . showString " ≤ " . showsPrec 6 y
    LTU x y -> showParen (prec > 5) $ showsPrec 6 x . showString " < " . showsPrec 6 y
    IfB p x y -> showParen (prec > 1) $ showString "if " . showsPrec 2 p . showString " then " . showsPrec 2 x . showString " else " . showsPrec 2 y

--------------------------------------------------------------------------------

-- | Smart constructor for 'Boolean True'
true :: Boolean
true = Boolean True

-- | Smart constructor for 'Boolean False'
false :: Boolean
false = Boolean False

-- | Set the i-th bit of a Unsigned integer with a Boolean
setBit :: KnownNat w => UInt w -> Int -> Boolean -> UInt w
setBit = SetU

-- | Modular multiplicative inverse of an Unsigned integer with a given modulus
--
--   @since 0.9.4.0
modInv :: KnownNat w => UInt w -> Integer -> UInt w
modInv = MMIU

-- | Greater than on Unsigned integers
--
--   @since 0.10.0
gt :: KnownNat w => UInt w -> UInt w -> Boolean
gt = GTU

-- | Greater than or equal on Unsigned integers
--
--   @since 0.10.0
gte :: KnownNat w => UInt w -> UInt w -> Boolean
gte = GTEU

-- | Less than on Unsigned integers
--
--   @since 0.10.0
lt :: KnownNat w => UInt w -> UInt w -> Boolean
lt = LTU

-- | Less than or equal on Unsigned integers
--
--   @since 0.10.0
lte :: KnownNat w => UInt w -> UInt w -> Boolean
lte = LTEU

-- | Fast exponentiation
--
--   @since 0.11.0
pow :: Field -> Integer -> Field
pow = Exp

-- | Hardcoded GF(256) Multiplication for AES
--
--   @since 0.17.0
aesMul :: UInt 8 -> UInt 8 -> UInt 8
aesMul = AESMulU

--------------------------------------------------------------------------------

-- | Typeclass for computing equality on values
class EQ a where
  -- | Equality
  eq :: a -> a -> Boolean

  -- | Inequality
  neq :: a -> a -> Boolean

instance EQ Boolean where
  eq = EqB
  neq x y = Not (x `eq` y)

instance EQ Field where
  eq = EqF
  neq x y = Not (x `eq` y)

instance KnownNat w => EQ (UInt w) where
  eq = EqU
  neq x y = Not (x `eq` y)

--------------------------------------------------------------------------------

-- | A "Variable" is just a synonym for an 'Int'
type Var = Int

-- | Bit width
type Width = Int
