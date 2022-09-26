{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Module for converting Kinded syntax to Typed syntax
module Keelung.Syntax.Simplify (simplify, simplifyM, simplifyComputation) where

import Control.Monad.Reader
import qualified Data.Array.Unboxed as Array
import qualified Data.IntMap as IntMap
import qualified Keelung.Monad as Kinded
import qualified Keelung.Syntax.Kinded as Kinded
import Keelung.Syntax.Typed
import Keelung.Types (Addr, Heap)
import qualified Keelung.Types as Kinded

--------------------------------------------------------------------------------

-- | Monad for storing the Heap
type HeapM = Reader Heap

runHeapM :: Heap -> HeapM a -> a
runHeapM h m = runReader m h

readArray :: Addr -> Int -> HeapM Expr
readArray addr len = Array <$> mapM (readHeap addr) indices
  where
    indices :: Array.Array Int Int
    indices = Array.listArray (0, pred len) [0 .. pred len]

    readHeap :: Addr -> Int -> HeapM Expr
    readHeap addr' i = do
      heap <- ask
      case IntMap.lookup addr' heap of
        Nothing -> error "HeapM: address not found"
        Just (elemType, array) -> case IntMap.lookup i array of
          Nothing -> error "HeapM: index ouf of bounds"
          Just addr'' -> case elemType of
            Kinded.NumElem -> return $ Var $ NumVar addr''
            Kinded.BoolElem -> return $ Var $ BoolVar addr''
            Kinded.ArrElem _ len' -> readArray addr'' len'

--------------------------------------------------------------------------------

simplifyComputation :: Kinded.Computation -> Computation
simplifyComputation (Kinded.Computation nextVar nextInputVar nextAddr heap asgns bsgns asgns') =
  runHeapM heap $ do
    Computation
      nextVar
      nextInputVar
      nextAddr
      heap
      <$> mapM simplifyM asgns
      <*> mapM simplifyM bsgns
      <*> mapM simplifyM asgns'

simplify :: Simplify t Expr => Kinded.Elaborated t -> Elaborated
simplify (Kinded.ElaboratedNum expr comp) =
  let comp' = simplifyComputation comp
   in Elaborated
        (runHeapM (compHeap comp') (simplifyM expr))
        comp'
simplify (Kinded.ElaboratedBool expr comp) =
  let comp' = simplifyComputation comp
   in Elaborated
        (runHeapM (compHeap comp') (simplifyM expr))
        comp'
simplify (Kinded.ElaboratedArray expr comp) =
  let comp' = simplifyComputation comp
   in Elaborated
        (runHeapM (compHeap comp') (simplifyM expr))
        comp'

simplify (Kinded.ElaboratedUnit expr comp) =
  let comp' = simplifyComputation comp
   in Elaborated
        (runHeapM (compHeap comp') (simplifyM expr))
        comp'

--------------------------------------------------------------------------------

-- | Typeclass for removing kinds
class Simplify a b where
  simplifyM :: a -> HeapM b

instance Simplify Kinded.Number Expr where
  simplifyM expr = case expr of
    Kinded.Integer n -> return $ Val (Integer n)
    Kinded.Rational n -> return $ Val (Rational n)
    Kinded.NumberRef var -> return $ Var (NumVar var)
    Kinded.Add x y -> Add <$> simplifyM x <*> simplifyM y
    Kinded.Sub x y -> Sub <$> simplifyM x <*> simplifyM y
    Kinded.Mul x y -> Mul <$> simplifyM x <*> simplifyM y
    Kinded.Div x y -> Div <$> simplifyM x <*> simplifyM y
    Kinded.IfNum p x y -> If <$> simplifyM p <*> simplifyM x <*> simplifyM y
    Kinded.ToNum x -> ToNum <$> simplifyM x

instance Simplify Kinded.Boolean Expr where
  simplifyM expr = case expr of
    Kinded.Boolean b -> return $ Val (Boolean b)
    Kinded.BooleanRef var -> return $ Var (BoolVar var)
    Kinded.Eq x y -> Eq <$> simplifyM x <*> simplifyM y
    Kinded.And x y -> And <$> simplifyM x <*> simplifyM y
    Kinded.Or x y -> Or <$> simplifyM x <*> simplifyM y
    Kinded.Xor x y -> Xor <$> simplifyM x <*> simplifyM y
    Kinded.BEq x y -> BEq <$> simplifyM x <*> simplifyM y
    Kinded.IfBool p x y -> If <$> simplifyM p <*> simplifyM x <*> simplifyM y
    Kinded.ToBool x -> ToBool <$> simplifyM x

instance Simplify Kinded.Unit Expr where
  simplifyM expr = case expr of
    Kinded.Unit -> return $ Val Unit

instance Simplify t Expr => Simplify (Kinded.Arr t) Expr where
  simplifyM expr = case expr of
    Kinded.Arr xs -> Array <$> mapM simplifyM xs

-- instance Simplify (Kinded.Val t) Expr where
--   simplifyM expr = case expr of
--     Kinded.Integer n -> return $ Val (Integer n)
--     Kinded.Rational n -> return $ Val (Rational n)
--     Kinded.Boolean b -> return $ Val (Boolean b)
--     Kinded.UnitVal -> return $ Val Unit
--     Kinded.ArrayVal xs -> Array <$> mapM simplifyM xs
--     Kinded.Ref x -> case x of
--       Kinded.BoolVar n -> return $ Var (BoolVar n)
--       Kinded.BoolInputVar n -> return $ Var (BoolInputVar n)
--       Kinded.NumVar n -> return $ Var (NumVar n)
--       Kinded.NumInputVar n -> return $ Var (NumInputVar n)
--       Kinded.ArrayRef _ len addr -> readArray addr len
--     Kinded.Add x y -> Add <$> simplifyM x <*> simplifyM y
--     Kinded.Sub x y -> Sub <$> simplifyM x <*> simplifyM y
--     Kinded.Mul x y -> Mul <$> simplifyM x <*> simplifyM y
--     Kinded.Div x y -> Div <$> simplifyM x <*> simplifyM y
--     Kinded.Eq x y -> Eq <$> simplifyM x <*> simplifyM y
--     Kinded.And x y -> And <$> simplifyM x <*> simplifyM y
--     Kinded.Or x y -> Or <$> simplifyM x <*> simplifyM y
--     Kinded.Xor x y -> Xor <$> simplifyM x <*> simplifyM y
--     Kinded.BEq x y -> BEq <$> simplifyM x <*> simplifyM y
--     Kinded.IfNum p x y -> If <$> simplifyM p <*> simplifyM x <*> simplifyM y
--     Kinded.IfBool p x y -> If <$> simplifyM p <*> simplifyM x <*> simplifyM y
--     Kinded.ToBool x -> ToBool <$> simplifyM x
--     Kinded.ToNum x -> ToNum <$> simplifyM x

instance Simplify Kinded.Assignment Assignment where
  simplifyM (Kinded.BoolAssignment var e) = Assignment (NumVar var) <$> simplifyM e
  simplifyM (Kinded.NumAssignment var e) = Assignment (BoolVar var) <$> simplifyM e

-- instance Simplify (Kinded.Assignment 'Kinded.Bool) Assignment where
--   simplifyM (Kinded.Assignment (Kinded.BoolVar n) e) = Assignment (BoolVar n) <$> simplifyM e
--   simplifyM (Kinded.Assignment (Kinded.BoolInputVar n) e) = Assignment (BoolInputVar n) <$> simplifyM e
