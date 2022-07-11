{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Keelung.Syntax.Concrete where

import Control.Arrow (left)
import Control.Monad.Except
import Control.Monad.State
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Serialize
import Data.Typeable
import GHC.Generics (Generic)
import Keelung.Error
import Keelung.Field
import qualified Keelung.Monad as S
import Keelung.Syntax (Addr, Heap, Var)
import qualified Keelung.Syntax as S

class Flatten a b where
  flatten :: a -> b

data Value
  = Number Integer
  | Boolean Bool
  | Unit
  deriving (Generic, Eq)

instance Show Value where
  show (Number n) = show n
  show (Boolean b) = show b
  show Unit = "unit"

instance Serialize Value

--------------------------------------------------------------------------------

data VarRef
  = NumVar Var
  | BoolVar Var
  | ArrVar Int Addr 
  deriving (Generic, Eq)

instance Serialize VarRef

instance Show VarRef where
  show (NumVar n) = "$N" ++ show n
  show (BoolVar n) = "$B" ++ show n
  show (ArrVar _ n) = "$A" ++ show n

--------------------------------------------------------------------------------
data ArrRef = Arr Addr ArrKind
  deriving (Generic, Eq)

instance Serialize ArrRef

instance Show ArrRef where
  show (Arr addr _) = "@" ++ show addr

data ArrKind = ArrOf | ArrOfNum | ArrOfBool | ArrOfUnit
  deriving (Generic, Eq)

instance Serialize ArrKind

-- instance Typeable kind => Flatten (S.Ref ('S.V kind)) VarRef where
--   flatten var@(S.Variable ref)
--     | typeOf var == typeRep (Proxy :: Proxy (S.Ref ('S.V 'S.Bool))) = BoolVar ref
--     | typeOf var == typeRep (Proxy :: Proxy (S.Ref ('S.V 'S.Num))) = NumVar ref
--     | otherwise = UnitVar ref

instance Typeable kind => Flatten (S.Ref kind) VarRef where
  flatten (S.Variable2Bool i) = BoolVar i
  flatten (S.Variable2Num i) = NumVar i 
  flatten (S.Array2 n i) = ArrVar n i 


data Expr
  = Val Value
  | Var VarRef 
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
  -- Var _ -> 1
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
  If x y z -> 1 + sizeOfExpr x + sizeOfExpr y + sizeOfExpr z
  ToBool x -> 1 + sizeOfExpr x
  ToNum x -> 1 + sizeOfExpr x

isOfUnit :: Expr -> Bool
isOfUnit (Val Unit) = True
isOfUnit _ = False

instance Show Expr where
  showsPrec prec expr = case expr of
    Val val -> shows val
    -- Var var -> shows var
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

instance (Typeable kind, Integral n) => Flatten (S.Expr kind n) Expr where
  flatten (S.Number n) = Val (Number (toInteger n))
  flatten (S.Boolean b) = Val (Boolean b)
  flatten S.UnitVal = Val Unit
  flatten (S.Ref ref) = Var (flatten ref)
  flatten (S.Add x y) = Add (flatten x) (flatten y)
  flatten (S.Sub x y) = Sub (flatten x) (flatten y)
  flatten (S.Mul x y) = Mul (flatten x) (flatten y)
  flatten (S.Div x y) = Div (flatten x) (flatten y)
  flatten (S.Eq x y) = Eq (flatten x) (flatten y)
  flatten (S.And x y) = And (flatten x) (flatten y)
  flatten (S.Or x y) = Or (flatten x) (flatten y)
  flatten (S.Xor x y) = Xor (flatten x) (flatten y)
  flatten (S.BEq x y) = BEq (flatten x) (flatten y)
  flatten (S.If c t e) = If (flatten c) (flatten t) (flatten e)
  flatten (S.ToBool x) = ToBool (flatten x)
  flatten (S.ToNum x) = ToNum (flatten x)

instance Serialize Expr

instance Serialize FieldType

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

-- instance (Typeable kind, Integral n) => Flatten (S.Computation n) Elaborated where
--   flatten (S.Computation e c) = Elaborated (flatten e) (flatten c)

instance
  (Typeable kind, Integral n, Flatten (S.Computation n) Computation) =>
  Flatten (S.Elaborated kind n) Elaborated
  where
  flatten (S.Elaborated e c) = Elaborated (flatten e) (flatten c)

-- instance (Typeable kind) => Flatten (S.Elaborated kind B64) Elaborated where
--   flatten (S.Elaborated e c) = Elaborated (flatten e) (flatten c)

-- instance (Typeable kind) => Flatten (S.Elaborated kind GF181) Elaborated where
--   flatten (S.Elaborated e c) = Elaborated (flatten e) (flatten c)

-- instance (Typeable kind) => Flatten (S.Elaborated kind BN128) Elaborated where
--   flatten (S.Elaborated e c) = Elaborated (flatten e) (flatten c)

instance Serialize Elaborated

-- | An Assignment associates an expression with a reference
data Assignment = Assignment VarRef Expr
  deriving (Generic)

instance Show Assignment where
  show (Assignment var expr) = show var <> " := " <> show expr

instance (Typeable kind, Integral n) => Flatten (S.Assignment kind n) Assignment where
  flatten (S.Assignment r e) = Assignment (flatten r) (flatten e)

instance Serialize Assignment

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

instance (Integral n, AcceptedField n) => Flatten (S.Computation n) Computation where
  flatten (S.Computation nextVar nextAddr inputVars heap asgns bsgns asgns') =
    Computation
      nextVar
      nextAddr
      inputVars
      heap
      (map flatten asgns)
      (map flatten bsgns)
      (map flatten asgns')
      (encodeFieldType $ toProxy asgns')
    where
      toProxy :: [S.Expr kind n] -> Proxy n
      toProxy = const Proxy

instance Serialize Computation

type Comp = StateT Computation (Except Error)

-- | How to run the 'Comp' monad
runComp :: Computation -> Comp a -> Either Error (a, Computation)
runComp comp f = runExcept (runStateT f comp)

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

assignNum :: VarRef -> Expr -> Comp ()
assignNum var e = modify' $ \st -> st {compNumAsgns = Assignment var e : compNumAsgns st}

assignBool :: VarRef -> Expr -> Comp ()
assignBool var e = modify' $ \st -> st {compBoolAsgns = Assignment var e : compNumAsgns st}

-- collect free variables of an expression
freeVars :: Expr -> IntSet
freeVars expr = case expr of
  Val _ -> mempty
  -- Var (NumVar n) -> IntSet.singleton n
  -- Var (BoolVar n) -> IntSet.singleton n
  -- Var (UnitVar n) -> IntSet.singleton n
  Var (NumVar n) -> IntSet.singleton n
  Var (BoolVar n) -> IntSet.singleton n
  Var (ArrVar _ n) -> IntSet.singleton n
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
