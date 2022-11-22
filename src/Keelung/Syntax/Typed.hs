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

data Val
  = Integer Integer
  | Rational Rational
  | Unsigned Int Integer
  | Boolean Bool
  | Unit
  deriving (Generic, Eq, NFData)

instance Show Val where
  show (Integer n) = show n
  show (Unsigned _ n) = show n
  show (Rational n) = show n
  show (Boolean b) = show b
  show Unit = "()"

instance Serialize Val

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

--------------------------------------------------------------------------------

data Expr
  = Val Val
  | Var Ref
  | Array (Array Int Expr)
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Eq Expr Expr
  | And Expr Expr
  | Or Expr Expr
  | Xor Expr Expr
  | RotateR Int Expr
  | BEq Expr Expr
  | If Expr Expr Expr
  | ToNum Expr
  | Bit Expr Int
  deriving (Generic, Eq, NFData)

sizeOfExpr :: Expr -> Int
sizeOfExpr expr = case expr of
  Val _ -> 1
  Var _ -> 1
  Array xs -> sum (fmap sizeOfExpr xs)
  Add x y -> 1 + sizeOfExpr x + sizeOfExpr y
  Sub x y -> 1 + sizeOfExpr x + sizeOfExpr y
  Mul x y -> 1 + sizeOfExpr x + sizeOfExpr y
  Div x y -> 1 + sizeOfExpr x + sizeOfExpr y
  Eq x y -> 1 + sizeOfExpr x + sizeOfExpr y
  And x y -> 1 + sizeOfExpr x + sizeOfExpr y
  Or x y -> 1 + sizeOfExpr x + sizeOfExpr y
  Xor x y -> 1 + sizeOfExpr x + sizeOfExpr y
  BEq x y -> 1 + sizeOfExpr x + sizeOfExpr y
  RotateR _ x -> 1 + sizeOfExpr x
  If x y z -> 1 + sizeOfExpr x + sizeOfExpr y + sizeOfExpr z
  ToNum x -> 1 + sizeOfExpr x
  Bit x _ -> 1 + sizeOfExpr x

isOfUnit :: Expr -> Bool
isOfUnit (Val Unit) = True
isOfUnit _ = False

instance Show Expr where
  showsPrec prec expr = case expr of
    Val val -> shows val
    Var var -> shows var
    Array xs -> showList (toList xs)
    Add x y -> showParen (prec > 6) $ showsPrec 6 x . showString " + " . showsPrec 7 y
    Sub x y -> showParen (prec > 6) $ showsPrec 6 x . showString " - " . showsPrec 7 y
    Mul x y -> showParen (prec > 7) $ showsPrec 7 x . showString " * " . showsPrec 8 y
    Div x y -> showParen (prec > 7) $ showsPrec 7 x . showString " / " . showsPrec 8 y
    Eq x y -> showParen (prec > 5) $ showsPrec 6 x . showString " = " . showsPrec 6 y
    And x y -> showParen (prec > 3) $ showsPrec 4 x . showString " ∧ " . showsPrec 3 y
    Or x y -> showParen (prec > 2) $ showsPrec 3 x . showString " ∨ " . showsPrec 2 y
    Xor x y -> showParen (prec > 4) $ showsPrec 5 x . showString " ⊕ " . showsPrec 4 y
    RotateR n x -> showParen (prec > 8) $ showString "ROTATE " . showsPrec 9 n . showString " " . showsPrec 9 x
    BEq x y -> showParen (prec > 5) $ showsPrec 6 x . showString " = " . showsPrec 6 y
    If p x y -> showParen (prec > 1) $ showString "if " . showsPrec 2 p . showString " then " . showsPrec 2 x . showString " else " . showsPrec 2 y
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