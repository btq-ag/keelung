{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Keelung.Syntax where

import Data.Field.Galois (GaloisField (..))
import Data.IntMap.Strict (IntMap)
import Data.Kind (Type)
import Data.Semiring (Ring (..), Semiring (..))
import Data.Serialize

--------------------------------------------------------------------------------

-- | A "Variable" is just a integer.
type Var = Int

-- | An "Address" is also just a integer.
type Addr = Int

-- | A Heap is an mapping of mappings of variables
type Heap = IntMap (IntMap Int)

--------------------------------------------------------------------------------

-- | Data kind for annotating the type of expressions.
data ValKind
  = Num -- Field numbers
  | Bool -- Booleans
  | Unit -- Unit
  | Arr ValKind -- Arrays
  deriving
    ( Show,
      Eq
    )

data Ref2 :: ValKind -> Type where
  Variable2Bool :: Var -> Ref2 'Bool
  Variable2Num :: Var -> Ref2 'Num
  Array2 :: Int -> Addr -> Ref2 ('Arr val) -- RefKinds to arrays
   
instance Eq (Ref2 kind) where
  Variable2Bool i == Variable2Bool j = i == j
  Variable2Num i == Variable2Num j = i == j
  Array2 _ addr == Array2 _ addr' = addr == addr'

instance Serialize (Ref2 'Num) where
  put (Variable2Num v) = putWord8 0 >> put v
  get = do
    tag <- getWord8
    case tag of
      0 -> Variable2Num <$> get
      _ -> fail "Invalid tag"

instance Serialize (Ref2 'Bool) where
  put (Variable2Bool v) = putWord8 1 >> put v
  get = do
    tag <- getWord8
    case tag of
      1 -> Variable2Bool <$> get
      _ -> fail "Invalid tag"

instance Serialize (Ref2 ('Arr val)) where
  put (Array2 n a) = putWord8 2 >> put n >> put a
  get = do
    tag <- getWord8
    case tag of
      2 -> Array2 <$> get <*> get
      _ -> fail "Invalid tag"

instance Show (Ref2 ref) where
  show (Variable2Bool v) = "$B" ++ show v
  show (Variable2Num v) = "$N" ++ show v
  show (Array2 n a) = "$A" ++ show n ++ ":" ++ show a

--------------------------------------------------------------------------------

-- | Expressions are indexed by 'ValKind' and parameterised by some field
data Expr :: ValKind -> Type -> Type where
  -- Value
  Number :: n -> Expr 'Num n -- Field numbers
  Boolean :: Bool -> Expr 'Bool n -- Booleans
  UnitVal :: Expr 'Unit n -- Unit
  -- Variable
  VarNum :: Ref2 'Num -> Expr 'Num n
  VarBool :: Ref2 'Bool -> Expr 'Bool n
  Arr2 :: Ref2 ('Arr val) -> Expr ('Arr val) n
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
  If :: Expr 'Bool n -> Expr val n -> Expr val n -> Expr val n
  -- Conversion between Booleans and Field numbers
  ToBool :: Expr 'Num n -> Expr 'Bool n
  ToNum :: Expr 'Bool n -> Expr 'Num n

instance Serialize n => Serialize (Expr 'Num n) where
  put expr = case expr of
    Number n -> putWord8 0 >> put n
    VarNum ref -> putWord8 11 >> put ref
    Add x y -> putWord8 20 >> put x >> put y
    Sub x y -> putWord8 21 >> put x >> put y
    Mul x y -> putWord8 22 >> put x >> put y
    Div x y -> putWord8 23 >> put x >> put y
    If x y z -> putWord8 40 >> put x >> put y >> put z
    ToNum x -> putWord8 51 >> put x
  get = do
    tag <- getWord8
    case tag of
      0 -> Number <$> get
      11 -> VarNum <$> get
      20 -> Add <$> get <*> get
      21 -> Sub <$> get <*> get
      22 -> Mul <$> get <*> get
      23 -> Div <$> get <*> get
      40 -> If <$> get <*> get <*> get
      51 -> ToNum <$> get
      _ -> error $ "Invalid expr tag 1 " ++ show tag

instance Serialize n => Serialize (Expr 'Bool n) where
  put expr = case expr of
    Boolean b -> putWord8 1 >> put b
    VarBool ref -> putWord8 12 >> put ref
    Eq x y -> putWord8 30 >> put x >> put y
    And x y -> putWord8 31 >> put x >> put y
    Or x y -> putWord8 32 >> put x >> put y
    Xor x y -> putWord8 33 >> put x >> put y
    BEq x y -> putWord8 34 >> put x >> put y
    If x y z -> putWord8 40 >> put x >> put y >> put z
    ToBool x -> putWord8 50 >> put x
  get = do
    tag <- getWord8
    case tag of
      1 -> Boolean <$> get
      12 -> VarBool <$> get
      30 -> Eq <$> get <*> get
      31 -> And <$> get <*> get
      32 -> Or <$> get <*> get
      33 -> Xor <$> get <*> get
      34 -> BEq <$> get <*> get
      40 -> If <$> get <*> get <*> get
      50 -> ToBool <$> get
      _ -> error $ "Invalid expr tag 2 " ++ show tag

instance Serialize n => Serialize (Expr 'Unit n) where
  put expr = case expr of
    UnitVal -> putWord8 2
    If x y z -> putWord8 40 >> put x >> put y >> put z
  get = do
    tag <- getWord8
    case tag of
      2 -> pure UnitVal
      _ -> error $ "Invalid expr tag 3 " ++ show tag

instance Functor (Expr ty) where
  fmap f expr = case expr of
    Number n -> Number (f n)
    Boolean b -> Boolean b
    UnitVal -> UnitVal
    VarNum ref -> VarNum ref
    VarBool ref -> VarBool ref
    Arr2 ref -> Arr2 ref
    Add x y -> Add (fmap f x) (fmap f y)
    Sub x y -> Sub (fmap f x) (fmap f y)
    Mul x y -> Mul (fmap f x) (fmap f y)
    Div x y -> Div (fmap f x) (fmap f y)
    Eq x y -> Eq (fmap f x) (fmap f y)
    And x y -> And (fmap f x) (fmap f y)
    Or x y -> Or (fmap f x) (fmap f y)
    Xor x y -> Xor (fmap f x) (fmap f y)
    BEq x y -> BEq (fmap f x) (fmap f y)
    If p x y -> If (fmap f p) (fmap f x) (fmap f y)
    ToBool x -> ToBool (fmap f x)
    ToNum x -> ToNum (fmap f x)

instance Show n => Show (Expr ty n) where
  showsPrec prec expr = case expr of
    Number n -> showsPrec prec n
    Boolean b -> showsPrec prec b
    UnitVal -> showString "unit"
    -- Var var -> shows var
    VarNum var -> shows var
    VarBool var -> shows var
    Arr2 var -> shows var
    Add x y -> showParen (prec > 6) $ showsPrec 6 x . showString " + " . showsPrec 7 y
    Sub x y -> showParen (prec > 6) $ showsPrec 6 x . showString " - " . showsPrec 7 y
    Mul x y -> showParen (prec > 7) $ showsPrec 7 x . showString " * " . showsPrec 8 y
    Div x y -> showParen (prec > 7) $ showsPrec 7 x . showString " / " . showsPrec 8 y
    Eq x y -> showParen (prec > 5) $ showsPrec 6 x . showString " = " . showsPrec 6 y
    And x y -> showParen (prec > 3) $ showsPrec 4 x . showString " ∧ " . showsPrec 3 y
    Or x y -> showParen (prec > 2) $ showsPrec 3 x . showString " ∨ " . showsPrec 2 y
    Xor x y -> showParen (prec > 4) $ showsPrec 5 x . showString " ⊕ " . showsPrec 4 y
    BEq x y -> showParen (prec > 5) $ showsPrec 6 x . showString " = " . showsPrec 6 y
    If p x y -> showParen (prec > 1) $ showString "if " . showsPrec 2 p . showString " then " . showsPrec 2 x . showString " else " . showsPrec 2 y
    ToBool x -> showString "ToBool " . showsPrec prec x
    ToNum x -> showString "ToNum " . showsPrec prec x

instance Eq n => Eq (Expr ty n) where
  a == b = case (a, b) of
    (Number n, Number m) -> n == m
    (Boolean x, Boolean y) -> x == y
    (UnitVal, UnitVal) -> True
    -- (Var x, Var y) -> x == y
    (Add x y, Add z w) -> x == z && y == w
    (Sub x y, Sub z w) -> x == z && y == w
    (Mul x y, Mul z w) -> x == z && y == w
    (Div x y, Div z w) -> x == z && y == w
    (Eq x y, Eq z w) -> x == z && y == w
    (And x y, And z w) -> x == z && y == w
    (Or x y, Or z w) -> x == z && y == w
    (Xor x y, Xor z w) -> x == z && y == w
    (BEq x y, BEq z w) -> x == z && y == w
    (If x y z, If u v w) -> x == u && y == v && z == w
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
neq x y = If (x `Eq` y) false true

-- | Helper function for not-`BEq`
nbeq :: Expr 'Bool n -> Expr 'Bool n -> Expr 'Bool n
nbeq x y = If (x `BEq` y) false true

-- | Helper function for negating a boolean expression
neg :: Expr 'Bool n -> Expr 'Bool n
neg x = true `Xor` x

cond :: Expr 'Bool n -> Expr kind n -> Expr kind n -> Expr kind n
cond = If
