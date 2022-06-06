{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Keelung.Syntax.Unkinded where

import Control.Arrow (left)
import Control.Monad.Except
import Control.Monad.State
import Data.IntSet (IntSet)
import Data.Serialize
import Data.Typeable
import GHC.Generics (Generic)
import Keelung.Error
import qualified Keelung.Monad as S
import Keelung.Syntax (Addr, Heap, Var)
import qualified Keelung.Syntax as S
import qualified Data.IntSet as IntSet
import Keelung.Field (N(N))

class Flatten a b where
  flatten :: a -> b

data Value n = Number n | Boolean Bool | Unit
  deriving (Generic, Eq, Functor)

instance Show n => Show (Value n) where
  show (Number n) = show n
  show (Boolean b) = show b
  show Unit = "unit"

instance Flatten (S.Value kind n) (Value n) where
  flatten (S.Number n) = Number n
  flatten (S.Boolean b) = Boolean b
  flatten S.UnitVal = Unit

instance Serialize n => Serialize (Value n)

-- data Ref = Variable Var | Array Addr
--   deriving (Generic)

data VarRef
  = NumVar Var
  | BoolVar Var
  | UnitVar Var
  deriving (Generic, Eq)

instance Serialize VarRef

instance Show VarRef where
  show (NumVar n) = "$N" ++ show n
  show (BoolVar n) = "$B" ++ show n
  show (UnitVar n) = "$U" ++ show n

varRef :: VarRef -> Var
varRef (NumVar ref) = ref
varRef (BoolVar ref) = ref
varRef (UnitVar ref) = ref

data ArrRef = Arr Addr ArrKind
  deriving (Generic, Eq)

instance Serialize ArrRef

instance Show ArrRef where
  show (Arr addr _) = "@" ++ show addr

data ArrKind = ArrOf | ArrOfNum | ArrOfBool | ArrOfUnit
  deriving (Generic, Eq)

instance Serialize ArrKind

data Ref
  = VarRef VarRef
  | ArrRef ArrRef
  deriving (Generic)

instance Show Ref where
  show (VarRef ref) = show ref 
  show (ArrRef ref) = show ref 

instance Serialize Ref

instance Typeable kind => Flatten (S.Ref ('S.V kind)) VarRef where
  flatten var@(S.Variable ref)
    | typeOf var == typeRep (Proxy :: Proxy (S.Ref ('S.V 'S.Bool))) = BoolVar ref
    | typeOf var == typeRep (Proxy :: Proxy (S.Ref ('S.V 'S.Num))) = NumVar ref
    | otherwise = UnitVar ref

data Expr n
  = Val (Value n)
  | Var VarRef
  | Add (Expr n) (Expr n)
  | Sub (Expr n) (Expr n)
  | Mul (Expr n) (Expr n)
  | Div (Expr n) (Expr n)
  | Eq (Expr n) (Expr n)
  | And (Expr n) (Expr n)
  | Or (Expr n) (Expr n)
  | Xor (Expr n) (Expr n)
  | BEq (Expr n) (Expr n)
  | IfThenElse (Expr n) (Expr n) (Expr n)
  | ToBool (Expr n)
  | ToNum (Expr n)
  deriving (Generic, Eq, Functor)

sizeOfExpr :: Expr n -> Int
sizeOfExpr expr = case expr of
  Val _ -> 1
  Var _ -> 1
  Add x y -> 1 + sizeOfExpr x + sizeOfExpr y
  Sub x y -> 1 + sizeOfExpr x + sizeOfExpr y
  Mul x y -> 1 + sizeOfExpr x + sizeOfExpr y
  Div x y -> 1 + sizeOfExpr x + sizeOfExpr y
  Eq x y -> 1 + sizeOfExpr x + sizeOfExpr y
  And x y -> 1 + sizeOfExpr x + sizeOfExpr y
  Or x y -> 1 + sizeOfExpr x + sizeOfExpr y
  Xor x y -> 1 + sizeOfExpr x + sizeOfExpr y
  BEq x y -> 1 + sizeOfExpr x + sizeOfExpr y
  IfThenElse x y z -> 1 + sizeOfExpr x + sizeOfExpr y + sizeOfExpr z
  ToBool x -> 1 + sizeOfExpr x
  ToNum x -> 1 + sizeOfExpr x

isOfUnit :: Expr n -> Bool
isOfUnit (Val Unit) = True
isOfUnit (Var (UnitVar _)) = True
isOfUnit _ = False 

instance Show n => Show (Expr n) where
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
    IfThenElse p x y -> showParen (prec > 1) $ showString "if " . showsPrec 2 p . showString " then " . showsPrec 2 x . showString " else " . showsPrec 2 y
    ToBool x -> showString "ToBool " . showsPrec prec x
    ToNum x -> showString "ToNum " . showsPrec prec x

instance Typeable kind => Flatten (S.Expr kind n) (Expr n) where
  flatten (S.Val val) = Val (flatten val)
  flatten (S.Var ref) = Var (flatten ref)
  flatten (S.Add x y) = Add (flatten x) (flatten y)
  flatten (S.Sub x y) = Sub (flatten x) (flatten y)
  flatten (S.Mul x y) = Mul (flatten x) (flatten y)
  flatten (S.Div x y) = Div (flatten x) (flatten y)
  flatten (S.Eq x y) = Eq (flatten x) (flatten y)
  flatten (S.And x y) = And (flatten x) (flatten y)
  flatten (S.Or x y) = Or (flatten x) (flatten y)
  flatten (S.Xor x y) = Xor (flatten x) (flatten y)
  flatten (S.BEq x y) = BEq (flatten x) (flatten y)
  flatten (S.IfThenElse c t e) = IfThenElse (flatten c) (flatten t) (flatten e)
  flatten (S.ToBool x) = ToBool (flatten x)
  flatten (S.ToNum x) = ToNum (flatten x)

instance Serialize n => Serialize (Expr n)

data Elaborated n = Elaborated
  { -- | The resulting 'Expr'
    elabExpr :: !(Expr n),
    -- | The state of computation after elaboration
    elabComp :: Computation n
  }
  deriving (Generic)

instance (Show n, Bounded n, Integral n, Fractional n) => Show (Elaborated n) where
  show (Elaborated expr comp) =
    "{\n expression: "
      ++ show (fmap N expr)
      ++ "\n  compuation state: \n"
      ++ show comp
      ++ "\n}"

instance Typeable kind => Flatten (S.Elaborated kind n) (Elaborated n) where
  flatten (S.Elaborated e c) = Elaborated (flatten e) (flatten c)

instance Serialize n => Serialize (Elaborated n)

-- | An Assignment associates an expression with a reference
data Assignment n = Assignment VarRef (Expr n)
  deriving (Generic, Functor)

instance Show n => Show (Assignment n) where
  show (Assignment var expr) = show var <> " := " <> show expr

instance Typeable kind => Flatten (S.Assignment kind n) (Assignment n) where
  flatten (S.Assignment r e) = Assignment (flatten r) (flatten e)

instance Serialize n => Serialize (Assignment n)

-- | Data structure for elaboration bookkeeping
data Computation n = Computation
  { -- Counter for generating fresh variables
    compNextVar :: Int,
    -- Counter for allocating fresh heap addresses
    compNextAddr :: Int,
    -- Variables marked as inputs
    compInputVars :: IntSet,
    -- Heap for arrays
    compHeap :: Heap,
    -- Assignments
    compNumAsgns :: [Assignment n],
    compBoolAsgns :: [Assignment n],
    -- Assertions are expressions that are expected to be true
    compAssertions :: [Expr n]
  }
  deriving (Generic)

instance (Show n, Bounded n, Integral n, Fractional n) => Show (Computation n) where
  show (Computation nextVar nextAddr inputVars _ numAsgns boolAsgns assertions) =
    "{\n  variable counter: " ++ show nextVar
      ++ "\n  address counter: "
      ++ show nextAddr
      ++ "\n  input variables: "
      ++ show (IntSet.toList inputVars)
      ++ "\n  num assignments: "
      ++ show (map (fmap N) numAsgns)
      ++ "\n  bool assignments: "
      ++ show (map (fmap N) boolAsgns)
      ++ "\n  assertions: "
      ++ show (map (fmap N) assertions)
      ++ "\n\
         \}"

instance Flatten (S.Computation n) (Computation n) where
  flatten (S.Computation nextVar nextAddr inputVars heap asgns bsgns asgns') =
    Computation nextVar nextAddr inputVars heap (map flatten asgns) (map flatten bsgns) (map flatten asgns')

instance Serialize n => Serialize (Computation n)

type Comp n = StateT (Computation n) (Except Error)

-- | How to run the 'Comp' monad
runComp :: Computation n -> Comp n a -> Either Error (a, Computation n)
runComp comp f = runExcept (runStateT f comp)

-- | An alternative to 'elaborate' that returns '()' instead of 'Expr'
-- elaborate_ :: Comp n () -> Either String (Elaborated n)
-- elaborate_ prog = do
--   ((), comp') <- left show $ runComp (Computation 0 0 mempty mempty mempty mempty mempty) prog
--   return $ Elaborated Nothing comp'

elaborate :: Comp n (Expr n) -> Either String (Elaborated n)
elaborate prog = do
  (expr, comp') <- left show $ runComp (Computation 0 0 mempty mempty mempty mempty mempty) prog
  return $ Elaborated expr comp'

-- | Allocate a fresh variable.
allocVar :: Comp n Int
allocVar = do
  index <- gets compNextVar
  modify (\st -> st {compNextVar = succ index})
  return index

assignNum :: VarRef -> Expr n -> Comp n ()
assignNum var e = modify' $ \st -> st {compNumAsgns = Assignment var e : compNumAsgns st}

assignBool :: VarRef -> Expr n -> Comp n ()
assignBool var e = modify' $ \st -> st {compBoolAsgns = Assignment var e : compNumAsgns st}