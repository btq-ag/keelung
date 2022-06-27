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
  = -- | Field numbers
    Num
  | -- | Booleans
    Bool
  | -- | Unit
    Unit
  deriving
    ( Show,
      Eq
    )

-- | Data kind for annotating the type of references to variables and arrays.
data RefKind
  = -- | Variables of some 'ValKind'
    V ValKind
  | -- | Arrays of some 'RefKind'
    A RefKind
  deriving (Show)

--------------------------------------------------------------------------------

-- | Values are indexed by 'ValKind' and parameterised by some field
data Value :: ValKind -> Type -> Type where
  Number :: n -> Value 'Num n -- Field numbers
  Boolean :: Bool -> Value 'Bool n -- Booleans
  UnitVal :: Value 'Unit n -- Unit

instance Show n => Show (Value ty n) where
  show (Number n) = show n
  show (Boolean b) = show b
  show UnitVal = "unit"

instance Eq n => Eq (Value ty n) where
  Number n == Number m = n == m
  Boolean b == Boolean c = b == c
  UnitVal == UnitVal = True

instance Serialize n => Serialize (Value 'Num n) where
  put (Number n) = putWord8 0 >> put n
  get = do
    tag <- getWord8
    case tag of
      0 -> Number <$> get
      _ -> error "Invalid value tag"

instance Serialize n => Serialize (Value 'Bool n) where
  put (Boolean n) = putWord8 1 >> put n
  get = do
    tag <- getWord8
    case tag of
      1 -> Boolean <$> get
      _ -> error "Invalid value tag"

instance Serialize n => Serialize (Value 'Unit n) where
  put UnitVal = putWord8 2
  get = do
    tag <- getWord8
    case tag of
      2 -> pure UnitVal
      _ -> error "Invalid value tag"

--------------------------------------------------------------------------------

-- | RefKind values are indexed by 'RefKind'
data Ref :: RefKind -> Type where
  Variable :: Var -> Ref ('V val) -- RefKinds to variables
  Array :: Addr -> Ref ('A val) -- RefKinds to arrays

instance Show (Ref ref) where
  show (Variable i) = "$" <> show i
  show (Array addr) = "@" <> show addr

instance Eq (Ref ref) where
  Variable i == Variable j = i == j
  Array addr == Array addr' = addr == addr'

instance Serialize (Ref ('V val)) where
  put (Variable i) = putWord8 0 >> put i
  get = do
    tag <- getWord8
    case tag of
      0 -> Variable <$> get
      _ -> error "Invalid ref tag"

instance Serialize (Ref ('A val)) where
  put (Array addr) = putWord8 1 >> put addr
  get = do
    tag <- getWord8
    case tag of
      1 -> Array <$> get
      _ -> error "Invalid ref tag"

--------------------------------------------------------------------------------

-- | Expressions are indexed by 'ValKind' and parameterised by some field
data Expr :: ValKind -> Type -> Type where
  -- Value
  Val :: Value val n -> Expr val n
  -- Variable
  Var :: Ref ('V val) -> Expr val n
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
    Val val -> putWord8 0 >> put val
    Var ref -> putWord8 1 >> put ref
    Add x y -> putWord8 2 >> put x >> put y
    Sub x y -> putWord8 3 >> put x >> put y
    Mul x y -> putWord8 4 >> put x >> put y
    Div x y -> putWord8 5 >> put x >> put y
    If x y z -> putWord8 11 >> put x >> put y >> put z
    ToNum x -> putWord8 13 >> put x
  get = do
    tag <- getWord8
    case tag of
      0 -> Val <$> get
      1 -> Var <$> get
      2 -> Add <$> get <*> get
      3 -> Sub <$> get <*> get
      4 -> Mul <$> get <*> get
      5 -> Div <$> get <*> get
      11 -> If <$> get <*> get <*> get
      13 -> ToNum <$> get
      _ -> error $ "Invalid expr tag 1 " ++ show tag

instance Serialize n => Serialize (Expr 'Bool n) where
  put expr = case expr of 
    Val val -> putWord8 0 >> put val
    Var ref -> putWord8 1 >> put ref
    Eq x y -> putWord8 6 >> put x >> put y
    And x y -> putWord8 7 >> put x >> put y
    Or x y -> putWord8 8 >> put x >> put y
    Xor x y -> putWord8 9 >> put x >> put y
    BEq x y -> putWord8 10 >> put x >> put y
    If x y z -> putWord8 11 >> put x >> put y >> put z
    ToBool x -> putWord8 12 >> put x
  get = do
    tag <- getWord8
    case tag of
      0 -> Val <$> get
      1 -> Var <$> get
      6 -> Eq <$> get <*> get
      7 -> And <$> get <*> get
      8 -> Or <$> get <*> get
      9 -> Xor <$> get <*> get
      10 -> BEq <$> get <*> get
      11 -> If <$> get <*> get <*> get
      12 -> ToBool <$> get
      _ -> error $ "Invalid expr tag 2 " ++ show tag

instance Serialize n => Serialize (Expr 'Unit n) where
  put (Val val) = putWord8 0 >> put val
  put (Var ref) = putWord8 1 >> put ref
  put (If x y z) = putWord8 11 >> put x >> put y >> put z
  get = do
    tag <- getWord8
    case tag of
      0 -> pure (Val UnitVal)
      _ -> error $ "Invalid expr tag 3 " ++ show tag

instance Functor (Expr ty) where
  fmap f expr = case expr of
    Val val -> Val $ case val of
      Number a -> Number (f a)
      Boolean b -> Boolean b
      UnitVal -> UnitVal
    Var ref -> Var ref
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
    Val val -> shows val
    Var var -> shows var
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
    (Val x, Val y) -> x == y
    (Var x, Var y) -> x == y
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
  signum = const (Val (Number 1))
  fromInteger = Val . Number . fromNatural . fromInteger

instance GaloisField n => Semiring (Expr 'Num n) where
  plus = Add
  times = Mul
  zero = Val (Number 0)
  one = Val (Number 1)

instance GaloisField n => Ring (Expr 'Num n) where
  negate = id

instance GaloisField n => Fractional (Expr 'Num n) where
  fromRational = Val . Number . fromRational
  (/) = Div

--------------------------------------------------------------------------------

-- | An synonym of 'ToNum' for converting booleans to numbers
fromBool :: GaloisField n => Expr 'Bool n -> Expr 'Num n
fromBool = ToNum

-- | An synonym of 'ToBool' for converting numbers to booleans
toBool :: GaloisField n => Expr 'Num n -> Expr 'Bool n
toBool = ToBool

-- | Smart constructor for numbers as expressions
num :: n -> Expr 'Num n
num = Val . Number

-- | Smart constructor for 'True'
true :: Expr 'Bool n
true = Val (Boolean True)

-- | Smart constructor for 'False'
false :: Expr 'Bool n
false = Val (Boolean False)

-- | Smart constructor for 'Unit'
unit :: Expr 'Unit n
unit = Val UnitVal

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
