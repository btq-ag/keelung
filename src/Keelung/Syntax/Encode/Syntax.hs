{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Module for encoding Keelung programs
module Keelung.Syntax.Encode.Syntax where

import Control.DeepSeq (NFData)
import Data.Array.Unboxed (Array)
import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import Data.Sequence (Seq)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Keelung.Data.Struct
import Keelung.Field (FieldType)
import Keelung.Syntax (Var, Width)
import Keelung.Syntax.Counters

--------------------------------------------------------------------------------

-- | Booleans
data Boolean
  = -- | Boolean values
    ValB Bool
  | -- | Boolean variables
    VarB Var
  | -- | Boolean public input variables
    VarBI Var
  | -- | Boolean private input variables
    VarBP Var
  | -- | Conjunction
    AndB Boolean Boolean
  | -- | Disjunction
    OrB Boolean Boolean
  | -- | Exclusive disjunction
    XorB Boolean Boolean
  | -- | Complement
    NotB Boolean
  | -- | Conditional that returns a Boolean
    IfB Boolean Boolean Boolean
  | -- | Equality on Booleans
    EqB Boolean Boolean
  | -- | Equality on Field elements
    EqF Field Field
  | -- | Equality on Unsigned integers
    EqU Width UInt UInt
  | -- | Bit test on Unsigned integers
    BitU Width UInt Int
  deriving (Generic, Eq, NFData)

instance Serialize Boolean

instance Show Boolean where
  showsPrec prec expr = case expr of
    ValB n -> shows n
    VarB var -> showString "$B" . shows var
    VarBI var -> showString "$BI" . shows var
    VarBP var -> showString "$BP" . shows var
    AndB x y -> showParen (prec > 3) $ showsPrec 4 x . showString " ∧ " . showsPrec 3 y
    OrB x y -> showParen (prec > 2) $ showsPrec 3 x . showString " ∨ " . showsPrec 2 y
    XorB x y -> showParen (prec > 4) $ showsPrec 5 x . showString " ⊕ " . showsPrec 4 y
    NotB x -> showParen (prec > 8) $ showString "¬ " . showsPrec 9 x
    IfB p x y -> showParen (prec > 1) $ showString "if " . showsPrec 2 p . showString " then " . showsPrec 2 x . showString " else " . showsPrec 2 y
    EqB x y -> showParen (prec > 5) $ showsPrec 6 x . showString " = " . showsPrec 6 y
    EqF x y -> showParen (prec > 5) $ showsPrec 6 x . showString " = " . showsPrec 6 y
    EqU _ x y -> showParen (prec > 5) $ showsPrec 6 x . showString " = " . showsPrec 6 y
    BitU _ x i -> showParen (prec > 6) $ showsPrec 7 x . showString " [" . shows i . showString "]"

--------------------------------------------------------------------------------

-- | Field elements
data Field
  = -- | Integral values
    ValF Integer
  | -- | Rational values
    ValFR Rational
  | -- | Field element variables
    VarF Var
  | -- | Field element public input variables
    VarFI Var
  | -- | Field element private input variables
    VarFP Var
  | -- | Addition
    AddF Field Field
  | -- | Subtraction
    SubF Field Field
  | -- | Multiplication
    MulF Field Field
  | -- |  Division (without remainders)
    DivF Field Field
  | -- | Conditional that returns a Field element
    IfF Boolean Field Field
  | -- | Conversion from Boolean to Field element
    BtoF Boolean
  deriving (Generic, Eq, NFData)

instance Serialize Field

instance Show Field where
  showsPrec prec expr = case expr of
    ValF n -> shows n
    ValFR n -> shows n
    VarF var -> showString "$F" . shows var
    VarFI var -> showString "$FI" . shows var
    VarFP var -> showString "$FP" . shows var
    AddF x y -> showParen (prec > 6) $ showsPrec 6 x . showString " + " . showsPrec 7 y
    SubF x y -> showParen (prec > 6) $ showsPrec 6 x . showString " - " . showsPrec 7 y
    MulF x y -> showParen (prec > 7) $ showsPrec 7 x . showString " * " . showsPrec 8 y
    DivF x y -> showParen (prec > 7) $ showsPrec 7 x . showString " / " . showsPrec 8 y
    IfF p x y -> showParen (prec > 1) $ showString "if " . showsPrec 2 p . showString " then " . showsPrec 2 x . showString " else " . showsPrec 2 y
    BtoF x -> showString "B→F " . showsPrec prec x

--------------------------------------------------------------------------------

-- | Unsigned Integers
data UInt
  = -- | Unsigned integers values
    ValU Width Integer
  | -- | Unsigned integer variables
    VarU Width Var
  | -- | Unsigned integer public input variables
    VarUI Width Var
  | -- | Unsigned integer private input variables
    VarUP Width Var
  | -- | Addition
    AddU Width UInt UInt
  | -- | Subtraction
    SubU Width UInt UInt
  | -- | Multiplication
    MulU Width UInt UInt
  | -- | Bitwise conjunction
    AndU Width UInt UInt
  | -- | Bitwise disjunction
    OrU Width UInt UInt
  | -- | Bitwise exclusive disjunction
    XorU Width UInt UInt
  | -- | Bitwise complement
    NotU Width UInt
  | -- | Rotate left
    RoLU Width Int UInt
  | -- | Shift left
    ShLU Width Int UInt
  | -- | Set bit and return the result
    SetU Width UInt Int Boolean
  | -- | Conditional that returns an Unsigned integer
    IfU Width Boolean UInt UInt
  | -- | Conversion from Boolean to Unsigned integer
    BtoU Width Boolean
  deriving (Generic, Eq, NFData)

instance Serialize UInt

instance Show UInt where
  showsPrec prec expr = case expr of
    ValU _ n -> shows n
    VarU w var -> showString "$U" . showString (toSubscript w) . shows var
    VarUI w var -> showString "$UI" . showString (toSubscript w) . shows var
    VarUP w var -> showString "$UP" . showString (toSubscript w) . shows var
    AddU _ x y -> showParen (prec > 6) $ showsPrec 6 x . showString " + " . showsPrec 7 y
    SubU _ x y -> showParen (prec > 6) $ showsPrec 6 x . showString " - " . showsPrec 7 y
    MulU _ x y -> showParen (prec > 7) $ showsPrec 7 x . showString " * " . showsPrec 8 y
    AndU _ x y -> showParen (prec > 3) $ showsPrec 4 x . showString " ∧ " . showsPrec 3 y
    OrU _ x y -> showParen (prec > 2) $ showsPrec 3 x . showString " ∨ " . showsPrec 2 y
    XorU _ x y -> showParen (prec > 4) $ showsPrec 5 x . showString " ⊕ " . showsPrec 4 y
    NotU _ x -> showParen (prec > 8) $ showString "¬ " . showsPrec prec x
    RoLU _ n x -> showParen (prec > 8) $ showString "RoL " . showsPrec 9 n . showString " " . showsPrec 9 x
    ShLU _ n x -> showParen (prec > 8) $ showString "ShL " . showsPrec 9 n . showString " " . showsPrec 9 x
    SetU _ x i b -> showParen (prec > 8) $ showsPrec 9 x . showString "[" . shows i . showString "] := " . showsPrec 9 b
    IfU _ p x y -> showParen (prec > 1) $ showString "if " . showsPrec 2 p . showString " then " . showsPrec 2 x . showString " else " . showsPrec 2 y
    BtoU _ x -> showString "B→U " . showsPrec prec x
    where
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

--------------------------------------------------------------------------------

-- | Encoding of the Keelung syntax
data Expr
  = -- | Unit
    Unit
  | -- | Booleans
    Boolean Boolean
  | -- | Field element
    Field Field
  | -- | Unsigned integers
    UInt UInt
  | -- | Arrays
    Array (Array Int Expr)
  deriving (Generic, Eq, NFData)

instance Show Expr where
  showsPrec prec expr = case expr of
    Unit -> showString "()"
    Boolean bool -> showsPrec prec bool
    Field num -> showsPrec prec num
    UInt uint -> showsPrec prec uint
    Array xs -> showList (toList xs)

instance Serialize Expr

instance Serialize FieldType

--------------------------------------------------------------------------------

-- | Encoding of a Keelung program after elaboration
data Elaborated = Elaborated
  { -- | The resulting 'Expr'
    elabExpr :: !Expr,
    -- | The state of computation after elaboration
    elabComp :: Computation
  }
  deriving (Generic, NFData)

instance Show Elaborated where
  show (Elaborated expr comp) =
    "{\n  Expression: \n    "
      <> showExpr
      <> "\n"
      <> showExprBindings (compExprBindings comp)
      <> showAssertions (compAssertions comp)
      <> "}"
    where
      showExpr = case expr of
        Array xs -> prettyList2 4 (toList xs)
        _ -> show expr

      showExprBindings eb =
        if empty eb
          then ""
          else
            "  Bindings of expressions: \n"
              <> unlines (map ("    " <>) (prettyStruct "" eb))
              <> "\n"
      showAssertions assertions =
        if null assertions
          then ""
          else "  Assertions: \n" <> unlines (map (("    " <>) . show) assertions) <> "\n"

      prettyList2 :: Show a => Int -> [a] -> String
      prettyList2 n list = case list of
        [] -> "[]"
        [x] -> "[" <> show x <> "]"
        (x : xs) ->
          unlines $
            map (replicate n ' ' <>) $
              "" : "[ " <> show x : map (\y -> ", " <> show y) xs <> ["]"]

instance Serialize Elaborated

--------------------------------------------------------------------------------

-- | Data structure for elaboration bookkeeping
data Computation = Computation
  { -- Variable bookkeeping
    compCounters :: !Counters,
    -- Bindings from variables to expressions
    compExprBindings :: Struct (IntMap Field) (IntMap Boolean) (IntMap UInt),
    -- Assertions are expressions that are expected to be true
    compAssertions :: [Expr],
    -- DivMod relations: dividend = divisor * quotient + remainder
    compDivModRelsU :: IntMap [(UInt, UInt, UInt, UInt)],
    -- Store side effects of the computation in a sequence so that we can simulate them during interpretation
    compSideEffects :: Seq SideEffect
  }
  deriving (Show, Generic, NFData)

instance Serialize Computation

--------------------------------------------------------------------------------

data SideEffect
  = AssignmentF Var Field
  | AssignmentB Var Boolean
  | AssignmentU Width Var UInt
  | DivMod Width UInt UInt UInt UInt
  deriving (Show, Generic, NFData)

instance Serialize SideEffect
