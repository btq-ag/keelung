{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Keelung.Syntax.Unkinded where

import Data.IntSet (IntSet)
import Data.Serialize
import GHC.Generics (Generic)
import qualified Keelung.Monad as S
import Keelung.Syntax (Addr, Heap, Var)
import qualified Keelung.Syntax as S

class Flatten a b where 
    flatten :: a -> b


data Value n = Number n | Boolean Bool | Unit
  deriving (Generic)

instance Flatten (S.Value kind n) (Value n) where
  flatten (S.Number n) = Number n
  flatten (S.Boolean b) = Boolean b
  flatten S.UnitVal = Unit

instance Serialize n => Serialize (Value n)

data Ref = Variable Var | Array Addr
  deriving (Generic)

instance Flatten (S.Ref kind) Ref where
  flatten (S.Variable v) = Variable v
  flatten (S.Array a) = Array a

instance Serialize Ref

data Expr n
  = Val (Value n)
  | Var Ref
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
  deriving (Generic)

instance Flatten (S.Expr kind n) (Expr n) where
  flatten (S.Val v) = Val (flatten v)
  flatten (S.Var r) = Var (flatten r)
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
    elabExpr :: !(Maybe (Expr n)),
    -- | The state of computation after elaboration
    elabComp :: Computation n
    -- | The description of the underlying field
    -- elabFieldType :: 
  }
  deriving (Generic)

instance Flatten (S.Elaborated kind n) (Elaborated n) where
  flatten (S.Elaborated e c) = Elaborated (fmap flatten e) (flatten c)

instance Serialize n => Serialize (Elaborated n)

-- | An Assignment associates an expression with a reference
data Assignment n = Assignment Ref (Expr n)
  deriving (Generic)

instance Flatten (S.Assignment kind n) (Assignment n) where
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

instance Flatten (S.Computation n) (Computation n) where
  flatten (S.Computation nextVar nextAddr inputVars heap asgns bsgns asgns') =
    Computation nextVar nextAddr inputVars heap (map flatten asgns) (map flatten bsgns) (map flatten asgns')

instance Serialize n => Serialize (Computation n)
  