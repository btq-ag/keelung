{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Keelung.Syntax.Concrete where

import Control.Arrow (left)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Serialize
import Data.Typeable
import GHC.Generics (Generic)
import Keelung.Error
import Keelung.Field
import qualified Keelung.Monad as S
import qualified Keelung.Syntax as S
import Keelung.Types (Addr, Heap, Var)
import qualified Keelung.Types as S

class Flatten a b where
  flatten :: a -> b

--------------------------------------------------------------------------------

type HeapM = Reader Heap

runHeapM :: Heap -> HeapM a -> a
runHeapM h m = runReader m h

readHeap :: Addr -> Int -> HeapM Expr
readHeap addr i = do
  heap <- ask
  case IntMap.lookup addr heap of
    Nothing -> error "HeapM: address not found"
    Just (elemType, array) -> case IntMap.lookup i array of
      Nothing -> error "HeapM: index ouf of bounds"
      Just n -> case elemType of
        S.NumElem -> return $ Var $ NumVar n
        S.BoolElem -> return $ Var $ NumVar n
        S.ArrElem _ len -> do
          Array <$> mapM (readHeap addr) [0 .. pred len]

--------------------------------------------------------------------------------

-- | Typeclass for removing kinds
class Simplify a b where
  simplify :: a -> HeapM b

simplifyComputation :: (Integral n, AcceptedField n) => S.Computation n -> Computation
simplifyComputation (S.Computation nextVar nextAddr inputVars heap asgns bsgns asgns') =
  runHeapM heap $ do
    Computation
      nextVar
      nextAddr
      inputVars
      heap
      <$> mapM simplify asgns
      <*> mapM simplify bsgns
      <*> mapM simplify asgns'
      <*> return (encodeFieldType $ toProxy asgns')
  where
    toProxy :: [S.Val kind n] -> Proxy n
    toProxy = const Proxy

simplifyElaborated :: (Integral n, AcceptedField n) => S.Elaborated t n -> Elaborated
simplifyElaborated (S.Elaborated expr comp) =
  Elaborated
    (runHeapM heap (simplify expr))
    comp'
  where
    comp' = simplifyComputation comp
    heap = compHeap comp'

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
data ArrRef = Arr Addr ArrKind
  deriving (Generic, Eq)

instance Serialize ArrRef

instance Show ArrRef where
  show (Arr addr _) = "@" ++ show addr

data ArrKind = ArrOf | ArrOfNum | ArrOfBool | ArrOfUnit
  deriving (Generic, Eq)

instance Serialize ArrKind

-- instance Typeable kind => Flatten (S.Ref ('S.V kind)) Ref where
--   flatten var@(S.Variable ref)
--     | typeOf var == typeRep (Proxy :: Proxy (S.Ref ('S.V 'S.Bool))) = BoolVar ref
--     | typeOf var == typeRep (Proxy :: Proxy (S.Ref ('S.V 'S.Num))) = NumVar ref
--     | otherwise = UnitVar ref

-- instance Integral n => Flatten (S.Val t n) Val where
--   flatten (S.Number n) = Number (toInteger n)
--   flatten (S.Boolean b) = Boolean b
--   flatten S.UnitVal = Unit

instance Flatten (S.Ref 'S.Bool) Ref where
  flatten (S.BoolVar i) = BoolVar i

instance Flatten (S.Ref 'S.Num) Ref where
  flatten (S.NumVar i) = NumVar i

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

instance Integral n => Simplify (S.Val t n) Expr where
  simplify expr = case expr of
    S.Number n -> return $ Val (Number (toInteger n))
    S.Boolean b -> return $ Val (Boolean b)
    S.UnitVal -> return $ Val Unit
    S.Ref x -> case x of
      S.BoolVar n -> return $ Var (BoolVar n)
      S.NumVar n -> return $ Var (NumVar n)
      S.Array _ len addr -> Array <$> mapM (readHeap addr) [0 .. pred len]
    S.Add x y -> Add <$> simplify x <*> simplify y
    S.Sub x y -> Sub <$> simplify x <*> simplify y
    S.Mul x y -> Mul <$> simplify x <*> simplify y
    S.Div x y -> Div <$> simplify x <*> simplify y
    S.Eq x y -> Eq <$> simplify x <*> simplify y
    S.And x y -> And <$> simplify x <*> simplify y
    S.Or x y -> Or <$> simplify x <*> simplify y
    S.Xor x y -> Xor <$> simplify x <*> simplify y
    S.BEq x y -> BEq <$> simplify x <*> simplify y
    S.IfNum p x y -> If <$> simplify p <*> simplify x <*> simplify y
    S.IfBool p x y -> If <$> simplify p <*> simplify x <*> simplify y
    S.ToBool x -> ToBool <$> simplify x
    S.ToNum x -> ToNum <$> simplify x

instance Integral n => Flatten (S.Val 'S.Num n) Expr where
  flatten (S.Number n) = Val (Number (toInteger n))
  flatten (S.Ref ref) = Var (flatten ref)
  flatten (S.Add x y) = Add (flatten x) (flatten y)
  flatten (S.Sub x y) = Sub (flatten x) (flatten y)
  flatten (S.Mul x y) = Mul (flatten x) (flatten y)
  flatten (S.Div x y) = Div (flatten x) (flatten y)
  flatten (S.IfNum c t e) = If (flatten c) (flatten t) (flatten e)
  flatten (S.ToNum x) = ToNum (flatten x)

instance Integral n => Flatten (S.Val 'S.Bool n) Expr where
  flatten (S.Boolean b) = Val (Boolean b)
  flatten (S.Ref ref) = Var (flatten ref)
  flatten (S.Eq x y) = Eq (flatten x) (flatten y)
  flatten (S.And x y) = And (flatten x) (flatten y)
  flatten (S.Or x y) = Or (flatten x) (flatten y)
  flatten (S.Xor x y) = Xor (flatten x) (flatten y)
  flatten (S.BEq x y) = BEq (flatten x) (flatten y)
  flatten (S.IfBool c t e) = If (flatten c) (flatten t) (flatten e)
  flatten (S.ToBool x) = ToBool (flatten x)

instance Integral n => Flatten (S.Val 'S.Unit n) Expr where
  flatten S.UnitVal = Val Unit
  flatten (S.Ref ref) = case ref of {}

-- instance (Integral n) => Flatten (S.Expr kind n) Expr where
--   flatten (S.Val val) = Val (flatten val)
--   flatten (S.Ref ref) = Var (flatten ref)
--   flatten (S.Add x y) = Add (flatten x) (flatten y)
--   flatten (S.Sub x y) = Sub (flatten x) (flatten y)
--   flatten (S.Mul x y) = Mul (flatten x) (flatten y)
--   flatten (S.Div x y) = Div (flatten x) (flatten y)
--   flatten (S.Eq x y) = Eq (flatten x) (flatten y)
--   flatten (S.And x y) = And (flatten x) (flatten y)
--   flatten (S.Or x y) = Or (flatten x) (flatten y)
--   flatten (S.Xor x y) = Xor (flatten x) (flatten y)
--   flatten (S.BEq x y) = BEq (flatten x) (flatten y)
--   flatten (S.IfNum c t e) = If (flatten c) (flatten t) (flatten e)
--   flatten (S.IfBool c t e) = If (flatten c) (flatten t) (flatten e)
--   flatten (S.ToBool x) = ToBool (flatten x)
--   flatten (S.ToNum x) = ToNum (flatten x)

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

instance (Integral n, Flatten (S.Computation n) Computation) => Flatten (S.Elaborated 'S.Num n) Elaborated where
  flatten (S.Elaborated e c) = Elaborated (flatten e) (flatten c)

instance (Integral n, Flatten (S.Computation n) Computation) => Flatten (S.Elaborated 'S.Bool n) Elaborated where
  flatten (S.Elaborated e c) = Elaborated (flatten e) (flatten c)

instance (Integral n, Flatten (S.Computation n) Computation) => Flatten (S.Elaborated 'S.Unit n) Elaborated where
  flatten (S.Elaborated e c) = Elaborated (flatten e) (flatten c)

instance Serialize Elaborated

-- | An Assignment associates an expression with a reference
data Assignment = Assignment Ref Expr
  deriving (Generic)

instance Show Assignment where
  show (Assignment var expr) = show var <> " := " <> show expr

instance Integral n => Flatten (S.Assignment 'S.Num n) Assignment where
  flatten (S.Assignment r e) = Assignment (flatten r) (flatten e)

instance Integral n => Flatten (S.Assignment 'S.Bool n) Assignment where
  flatten (S.Assignment r e) = Assignment (flatten r) (flatten e)

instance Integral n => Simplify (S.Assignment 'S.Num n) Assignment where
  simplify (S.Assignment (S.NumVar n) e) = Assignment (NumVar n) <$> simplify e

instance Integral n => Simplify (S.Assignment 'S.Bool n) Assignment where
  simplify (S.Assignment (S.BoolVar n) e) = Assignment (BoolVar n) <$> simplify e

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
      toProxy :: [S.Val kind n] -> Proxy n
      toProxy = const Proxy

instance Serialize Computation

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
