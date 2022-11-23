{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Keelung.Syntax.Typed where

import Control.Arrow (left)
import Control.DeepSeq (NFData)
import Control.Monad.Except
import Control.Monad.State
import Data.Array.Unboxed (Array)
import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Keelung.Error (ElabError)
import Keelung.Field (FieldType)
import Keelung.Syntax.VarCounters
import Keelung.Types

--------------------------------------------------------------------------------

data Ref
  = VarN Var
  | InputVarN Var
  | VarB Var
  | InputVarB Var
  | VarU Int Var
  | InputVarU Int Var
  deriving (Generic, Eq, NFData)

instance Serialize Ref

instance Show Ref where
  show (VarN n) = "$N" <> show n
  show (InputVarN n) = "$N" <> show n
  show (VarB n) = "$B" <> show n
  show (InputVarB n) = "$B" <> show n
  show (VarU w n) = "$U[" <> show w <> "]" <> show n
  show (InputVarU w n) = "$U[" <> show w <> "]" <> show n

type Width = Int

--------------------------------------------------------------------------------

data Boolean
  = ValB Bool
  | AndB Boolean Boolean
  | OrB Boolean Boolean
  | XorB Boolean Boolean
  | IfB Boolean Boolean Boolean
  | LoopholeB Expr
  deriving (Generic, Eq, NFData)

instance Serialize Boolean

instance Show Boolean where
  showsPrec prec expr = case expr of
    ValB n -> shows n
    AndB x y -> showParen (prec > 3) $ showsPrec 4 x . showString " ∧ " . showsPrec 3 y
    OrB x y -> showParen (prec > 2) $ showsPrec 3 x . showString " ∨ " . showsPrec 2 y
    XorB x y -> showParen (prec > 4) $ showsPrec 5 x . showString " ⊕ " . showsPrec 4 y
    IfB p x y -> showParen (prec > 1) $ showString "if " . showsPrec 2 p . showString " then " . showsPrec 2 x . showString " else " . showsPrec 2 y
    -- NotU _ x -> showParen (prec > 8) $ showString "¬ " . showsPrec prec x
    LoopholeB _ -> error "LoopholeB"

--------------------------------------------------------------------------------

data Number
  = ValN Integer
  | ValNR Rational
  | AddN Number Number
  | SubN Number Number
  | MulN Number Number
  | DivN Number Number
  | IfN Boolean Number Number
  | LoopholeN Expr
  deriving (Generic, Eq, NFData)

instance Serialize Number

instance Show Number where
  showsPrec prec expr = case expr of
    ValN n -> shows n
    ValNR n -> shows n
    AddN x y -> showParen (prec > 6) $ showsPrec 6 x . showString " + " . showsPrec 7 y
    SubN x y -> showParen (prec > 6) $ showsPrec 6 x . showString " - " . showsPrec 7 y
    MulN x y -> showParen (prec > 7) $ showsPrec 7 x . showString " * " . showsPrec 8 y
    DivN x y -> showParen (prec > 7) $ showsPrec 7 x . showString " / " . showsPrec 8 y
    IfN p x y -> showParen (prec > 1) $ showString "if " . showsPrec 2 p . showString " then " . showsPrec 2 x . showString " else " . showsPrec 2 y
    LoopholeN _ -> error "LoopholeN"

--------------------------------------------------------------------------------

data UInt
  = ValU Width Integer
  | AddU Width UInt UInt
  | SubU Width UInt UInt
  | MulU Width UInt UInt
  | AndU Width UInt UInt
  | OrU Width UInt UInt
  | XorU Width UInt UInt
  | NotU Width UInt
  | IfU Width Boolean UInt UInt
  | LoopholeU Width Expr
  deriving (Generic, Eq, NFData)

instance Serialize UInt

instance Show UInt where
  showsPrec prec expr = case expr of
    ValU _ n -> shows n
    AddU _ x y -> showParen (prec > 6) $ showsPrec 6 x . showString " + " . showsPrec 7 y
    SubU _ x y -> showParen (prec > 6) $ showsPrec 6 x . showString " - " . showsPrec 7 y
    MulU _ x y -> showParen (prec > 7) $ showsPrec 7 x . showString " * " . showsPrec 8 y
    AndU _ x y -> showParen (prec > 3) $ showsPrec 4 x . showString " ∧ " . showsPrec 3 y
    OrU _ x y -> showParen (prec > 2) $ showsPrec 3 x . showString " ∨ " . showsPrec 2 y
    XorU _ x y -> showParen (prec > 4) $ showsPrec 5 x . showString " ⊕ " . showsPrec 4 y
    NotU _ x -> showParen (prec > 8) $ showString "¬ " . showsPrec prec x
    IfU _ p x y -> showParen (prec > 1) $ showString "if " . showsPrec 2 p . showString " then " . showsPrec 2 x . showString " else " . showsPrec 2 y
    LoopholeU _ _ -> error "LoopholeU"

--------------------------------------------------------------------------------

data Expr
  = Unit
  | Boolean Boolean
  | Number Number
  | UInt UInt
  | Var Ref
  | Array (Array Int Expr)
  | Eq Expr Expr
  | RotateR Int Expr
  | BEq Expr Expr
  | ToNum Expr
  | Bit Expr Int
  deriving (Generic, Eq, NFData)

instance Show Expr where
  showsPrec prec expr = case expr of
    Unit -> showString "()"
    Boolean bool -> shows bool
    Number num -> shows num
    UInt uint -> shows uint
    Var var -> shows var
    Array xs -> showList (toList xs)
    Eq x y -> showParen (prec > 5) $ showsPrec 6 x . showString " = " . showsPrec 6 y
    RotateR n x -> showParen (prec > 8) $ showString "ROTATE " . showsPrec 9 n . showString " " . showsPrec 9 x
    BEq x y -> showParen (prec > 5) $ showsPrec 6 x . showString " = " . showsPrec 6 y
    ToNum x -> showString "ToNum " . showsPrec prec x
    Bit x i -> shows x . showString "[" . shows i . showString "]"

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
    "{\n  expression: "
      <> showExpr
      <> "\n  compuation state: \n"
      <> indent (indent (show comp))
      <> "\n}"
    where
      showExpr = case expr of
        Array xs -> prettyList2 4 (toList xs)
        _ -> show expr

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

--------------------------------------------------------------------------------

-- | An Assignment associates an expression with a reference
data Assignment = Assignment Ref Expr
  deriving (Generic, NFData)

instance Show Assignment where
  show (Assignment var expr) = show var <> " := " <> show expr

instance Serialize Assignment

--------------------------------------------------------------------------------

-- | Data structure for elaboration bookkeeping
data Computation = Computation
  { -- Variable bookkeeping
    compVarCounters :: !VarCounters,
    -- Assignments
    compNumAsgns :: [Assignment],
    compBoolAsgns :: [Assignment],
    compUIntAsgns :: IntMap [Assignment],
    -- Assertions are expressions that are expected to be true
    compAssertions :: [Expr]
  }
  deriving (Generic, NFData)

instance Show Computation where
  show (Computation varCounters numAsgns boolAsgns uintAsgns assertions) =
    "{\n" <> indent (show varCounters)
      <> "\n  Number assignments: "
      <> prettyList2 8 numAsgns
      <> "\n  Boolean assignments: "
      <> prettyList2 8 boolAsgns
      <> "\n  Unsigned Int assignments: "
      <> prettyList2 8 (IntMap.elems uintAsgns)
      <> "\n  assertions: "
      <> prettyList2 8 assertions
      <> "\n\
         \}"

instance Serialize Computation

--------------------------------------------------------------------------------

type Comp = StateT Computation (Except ElabError)

-- | How to run the 'Comp' monad
runComp :: Computation -> Comp a -> Either ElabError (a, Computation)
runComp comp f = runExcept (runStateT f comp)

evalComp :: Computation -> Comp a -> Either ElabError a
evalComp comp f = runExcept (evalStateT f comp)

elaborate :: Comp Expr -> Either String Elaborated
elaborate prog = do
  (expr, comp') <- left show $ runComp (Computation mempty mempty mempty mempty mempty) prog
  return $ Elaborated expr comp'

-- | Allocate a fresh variable.
allocVar :: Comp Int
allocVar = do
  index <- gets (intermediateVarSize . compVarCounters)
  modify (\st -> st {compVarCounters = bumpIntermediateVar (compVarCounters st)})
  return index

assignNum :: Ref -> Expr -> Comp ()
assignNum var e = modify' $ \st -> st {compNumAsgns = Assignment var e : compNumAsgns st}

assignBool :: Ref -> Expr -> Comp ()
assignBool var e = modify' $ \st -> st {compBoolAsgns = Assignment var e : compBoolAsgns st}