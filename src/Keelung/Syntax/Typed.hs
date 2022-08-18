{-# LANGUAGE DeriveGeneric #-}

module Keelung.Syntax.Typed where

import Control.Arrow (left)
import Control.Monad.Except
import Control.Monad.State
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Keelung.Error (ElabError)
import Keelung.Field (FieldType)
import Keelung.Types (Heap, Var)

--------------------------------------------------------------------------------

data Val
  = Number Integer
  | Boolean Bool
  | Unit
  deriving (Generic, Eq)

instance Show Val where
  show (Number n) = show n
  show (Boolean b) = show b
  show Unit = "unit"

instance Serialize Val

--------------------------------------------------------------------------------

data Ref
  = NumVar Var
  | BoolVar Var
  deriving (Generic, Eq)

instance Serialize Ref

instance Show Ref where
  show (NumVar n) = "$N" ++ show n
  show (BoolVar n) = "$B" ++ show n

--------------------------------------------------------------------------------

data Expr
  = Val Val
  | Var Ref
  | Array [Expr]
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Eq Expr Expr
  | And Expr Expr
  | Or Expr Expr
  | Xor Expr Expr
  | BEq Expr Expr
  | If Expr Expr Expr
  | ToBool Expr
  | ToNum Expr
  deriving (Generic, Eq)

sizeOfExpr :: Expr -> Int
sizeOfExpr expr = case expr of
  Val _ -> 1
  Var _ -> 1
  Array xs -> sum (map sizeOfExpr xs)
  Add x y -> 1 + sizeOfExpr x + sizeOfExpr y
  Sub x y -> 1 + sizeOfExpr x + sizeOfExpr y
  Mul x y -> 1 + sizeOfExpr x + sizeOfExpr y
  Div x y -> 1 + sizeOfExpr x + sizeOfExpr y
  Eq x y -> 1 + sizeOfExpr x + sizeOfExpr y
  And x y -> 1 + sizeOfExpr x + sizeOfExpr y
  Or x y -> 1 + sizeOfExpr x + sizeOfExpr y
  Xor x y -> 1 + sizeOfExpr x + sizeOfExpr y
  BEq x y -> 1 + sizeOfExpr x + sizeOfExpr y
  If x y z -> 1 + sizeOfExpr x + sizeOfExpr y + sizeOfExpr z
  ToBool x -> 1 + sizeOfExpr x
  ToNum x -> 1 + sizeOfExpr x

isOfUnit :: Expr -> Bool
isOfUnit (Val Unit) = True
isOfUnit _ = False

instance Show Expr where
  showsPrec prec expr = case expr of
    Val val -> shows val
    Var var -> shows var
    Array xs -> showList xs
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

instance Serialize Expr

instance Serialize FieldType

--------------------------------------------------------------------------------

data Elaborated = Elaborated
  { -- | The resulting 'Expr'
    elabExpr :: !Expr,
    -- | The state of computation after elaboration
    elabComp :: Computation
  }
  deriving (Generic)

instance Show Elaborated where
  show (Elaborated expr comp) =
    "{\n expression: "
      ++ show expr
      ++ "\n  compuation state: \n"
      ++ show comp
      ++ "\n}"

instance Serialize Elaborated

--------------------------------------------------------------------------------

-- | An Assignment associates an expression with a reference
data Assignment = Assignment Ref Expr
  deriving (Generic)

instance Show Assignment where
  show (Assignment var expr) = show var <> " := " <> show expr

instance Serialize Assignment

--------------------------------------------------------------------------------

-- | Data structure for elaboration bookkeeping
data Computation = Computation
  { -- Counter for generating fresh variables
    compNextVar :: Int,
    -- Counter for allocating fresh heap addresses
    compNextAddr :: Int,
    -- Variables marked as inputs
    compInputVars :: IntSet,
    -- Heap for arrays
    compHeap :: Heap,
    -- Assignments
    compNumAsgns :: [Assignment],
    compBoolAsgns :: [Assignment],
    -- Assertions are expressions that are expected to be true
    compAssertions :: [Expr],
    compFieldType :: FieldType
  }
  deriving (Generic)

instance Show Computation where
  show (Computation nextVar nextAddr inputVars _ numAsgns boolAsgns assertions fieldType) =
    "{\n  variable counter: " ++ show nextVar
      ++ "\n  address counter: "
      ++ show nextAddr
      ++ "\n  input variables: "
      ++ show (IntSet.toList inputVars)
      ++ "\n  num assignments: "
      ++ show numAsgns
      ++ "\n  bool assignments: "
      ++ show boolAsgns
      ++ "\n  assertions: "
      ++ show assertions
      ++ "\n  field type: "
      ++ show fieldType
      ++ "\n\
         \}"

instance Serialize Computation

--------------------------------------------------------------------------------

type Comp = StateT Computation (Except ElabError)

-- | How to run the 'Comp' monad
runComp :: Computation -> Comp a -> Either ElabError (a, Computation)
runComp comp f = runExcept (runStateT f comp)

evalComp :: Computation -> Comp a -> Either ElabError a
evalComp comp f = runExcept (evalStateT f comp)

elaborate :: FieldType -> Comp Expr -> Either String Elaborated
elaborate fieldType prog = do
  (expr, comp') <- left show $ runComp (Computation 0 0 mempty mempty mempty mempty mempty fieldType) prog
  return $ Elaborated expr comp'

-- | Allocate a fresh variable.
allocVar :: Comp Int
allocVar = do
  index <- gets compNextVar
  modify (\st -> st {compNextVar = succ index})
  return index

assignNum :: Ref -> Expr -> Comp ()
assignNum var e = modify' $ \st -> st {compNumAsgns = Assignment var e : compNumAsgns st}

assignBool :: Ref -> Expr -> Comp ()
assignBool var e = modify' $ \st -> st {compBoolAsgns = Assignment var e : compBoolAsgns st}

-- collect free variables of an expression
freeVars :: Expr -> IntSet
freeVars expr = case expr of
  Val _ -> mempty
  Var (NumVar n) -> IntSet.singleton n
  Var (BoolVar n) -> IntSet.singleton n
  Array xs -> IntSet.unions (map freeVars xs)
  Add x y -> freeVars x <> freeVars y
  Sub x y -> freeVars x <> freeVars y
  Mul x y -> freeVars x <> freeVars y
  Div x y -> freeVars x <> freeVars y
  Eq x y -> freeVars x <> freeVars y
  And x y -> freeVars x <> freeVars y
  Or x y -> freeVars x <> freeVars y
  Xor x y -> freeVars x <> freeVars y
  BEq x y -> freeVars x <> freeVars y
  If x y z -> freeVars x <> freeVars y <> freeVars z
  ToBool x -> freeVars x
  ToNum x -> freeVars x
