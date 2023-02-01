{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Keelung.Syntax.Typed where

import Control.DeepSeq (NFData)
import Data.Array.Unboxed (Array)
import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Keelung.Data.Struct
import Keelung.Field (FieldType)
import Keelung.Syntax.Counters
import Keelung.Types

--------------------------------------------------------------------------------

data Boolean
  = ValB Bool
  | VarB Var
  | VarBI Var
  | AndB Boolean Boolean
  | OrB Boolean Boolean
  | XorB Boolean Boolean
  | NotB Boolean
  | IfB Boolean Boolean Boolean
  | EqB Boolean Boolean
  | EqF Field Field
  | EqU Width UInt UInt
  | BitU Width UInt Int
  deriving (Generic, Eq, NFData)

instance Serialize Boolean

instance Show Boolean where
  showsPrec prec expr = case expr of
    ValB n -> shows n
    VarB var -> showString "$B" . shows var
    VarBI var -> showString "$BI" . shows var
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

data Field
  = ValF Integer
  | ValFR Rational
  | VarF Var
  | VarFI Var
  | AddF Field Field
  | SubF Field Field
  | MulF Field Field
  | DivF Field Field
  | IfF Boolean Field Field
  | BtoF Boolean
  deriving (Generic, Eq, NFData)

instance Serialize Field

instance Show Field where
  showsPrec prec expr = case expr of
    ValF n -> shows n
    ValFR n -> shows n
    VarF var -> showString "$F" . shows var
    VarFI var -> showString "$FI" . shows var
    AddF x y -> showParen (prec > 6) $ showsPrec 6 x . showString " + " . showsPrec 7 y
    SubF x y -> showParen (prec > 6) $ showsPrec 6 x . showString " - " . showsPrec 7 y
    MulF x y -> showParen (prec > 7) $ showsPrec 7 x . showString " * " . showsPrec 8 y
    DivF x y -> showParen (prec > 7) $ showsPrec 7 x . showString " / " . showsPrec 8 y
    IfF p x y -> showParen (prec > 1) $ showString "if " . showsPrec 2 p . showString " then " . showsPrec 2 x . showString " else " . showsPrec 2 y
    BtoF x -> showString "B→F " . showsPrec prec x

--------------------------------------------------------------------------------

data UInt
  = ValU Width Integer
  | VarU Width Var
  | VarUI Width Var
  | AddU Width UInt UInt
  | SubU Width UInt UInt
  | MulU Width UInt UInt
  | AndU Width UInt UInt
  | OrU Width UInt UInt
  | XorU Width UInt UInt
  | NotU Width UInt
  | RoLU Width Int UInt
  | ShLU Width Int UInt
  | IfU Width Boolean UInt UInt
  | BtoU Width Boolean
  deriving (Generic, Eq, NFData)

instance Serialize UInt

instance Show UInt where
  showsPrec prec expr = case expr of
    ValU _ n -> shows n
    VarU w var -> showString "$U" . showString (toSubscript w) . shows var
    VarUI w var -> showString "$UI" . showString (toSubscript w) . shows var
    AddU _ x y -> showParen (prec > 6) $ showsPrec 6 x . showString " + " . showsPrec 7 y
    SubU _ x y -> showParen (prec > 6) $ showsPrec 6 x . showString " - " . showsPrec 7 y
    MulU _ x y -> showParen (prec > 7) $ showsPrec 7 x . showString " * " . showsPrec 8 y
    AndU _ x y -> showParen (prec > 3) $ showsPrec 4 x . showString " ∧ " . showsPrec 3 y
    OrU _ x y -> showParen (prec > 2) $ showsPrec 3 x . showString " ∨ " . showsPrec 2 y
    XorU _ x y -> showParen (prec > 4) $ showsPrec 5 x . showString " ⊕ " . showsPrec 4 y
    NotU _ x -> showParen (prec > 8) $ showString "¬ " . showsPrec prec x
    RoLU _ n x -> showParen (prec > 8) $ showString "RoL " . showsPrec 9 n . showString " " . showsPrec 9 x
    ShLU _ n x -> showParen (prec > 8) $ showString "ShL " . showsPrec 9 n . showString " " . showsPrec 9 x
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

data Expr
  = Unit
  | Boolean Boolean
  | Field Field
  | UInt UInt
  | Array (Array Int Expr)
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

instance Serialize Elaborated

-- | Prettify list of stuff
prettyList :: Show a => [a] -> [String]
prettyList [] = ["[]"]
prettyList [x] = ["[" <> show x <> "]"]
prettyList (x : xs) = "" : "[ " <> show x : map (\y -> ", " <> show y) xs <> ["]"]

prettyList2 :: Show a => Int -> [a] -> String
prettyList2 n list = case list of
  [] -> "[]"
  [x] -> "[" <> show x <> "]"
  (x : xs) ->
    unlines $
      map (replicate n ' ' <>) $
        "" : "[ " <> show x : map (\y -> ", " <> show y) xs <> ["]"]

prettyList3 :: Int -> [String] -> String
prettyList3 n list = case list of
  [] -> ""
  [x] -> "[" <> x <> "]\n"
  (x : xs) ->
    unlines $
      map (replicate n ' ' <>) $
        "" : "[ " <> x : map (", " <>) xs <> ["]\n"]

--------------------------------------------------------------------------------

-- | Data structure for elaboration bookkeeping
data Computation = Computation
  { -- Variable bookkeeping
    compCounters :: !Counters,
    -- Bindings from variables to expressions
    compExprBindings :: Struct (IntMap Field) (IntMap Boolean) (IntMap UInt),
    -- Assertions are expressions that are expected to be true
    compAssertions :: [Expr],
    -- DivMod relations 
    compDivModRelsU :: IntMap (UInt, UInt, UInt, UInt)
  }
  deriving (Show, Generic, NFData)

instance Serialize Computation
