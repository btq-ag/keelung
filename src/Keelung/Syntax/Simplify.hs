-- | Module for converting Kinded syntax to Typed syntax 

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Keelung.Syntax.Simplify (simplify) where

import Control.Monad.Reader
import qualified Data.IntMap as IntMap
import Data.Proxy (Proxy (Proxy))
import Keelung.Field (AcceptedField, encodeFieldType)
import qualified Keelung.Monad as S
import qualified Keelung.Syntax as S
import Keelung.Syntax.Typed
import Keelung.Types (Addr, Heap)
import qualified Keelung.Types as S

--------------------------------------------------------------------------------

-- | Monad for storing the Heap
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

simplifyComputation :: (Integral n, AcceptedField n) => S.Computation n -> Computation
simplifyComputation (S.Computation nextVar nextAddr inputVars heap asgns bsgns asgns') =
  runHeapM heap $ do
    Computation
      nextVar
      nextAddr
      inputVars
      heap
      <$> mapM simplifyM asgns
      <*> mapM simplifyM bsgns
      <*> mapM simplifyM asgns'
      <*> return (encodeFieldType $ toProxy asgns')
  where
    toProxy :: [S.Val kind n] -> Proxy n
    toProxy = const Proxy

simplify :: (Integral n, AcceptedField n) => S.Elaborated t n -> Elaborated
simplify (S.Elaborated expr comp) =
  Elaborated
    (runHeapM heap (simplifyM expr))
    comp'
  where
    comp' = simplifyComputation comp
    heap = compHeap comp'

--------------------------------------------------------------------------------

-- | Typeclass for removing kinds
class Simplify a b where
  simplifyM :: a -> HeapM b

instance Integral n => Simplify (S.Val t n) Expr where
  simplifyM expr = case expr of
    S.Number n -> return $ Val (Number (toInteger n))
    S.Boolean b -> return $ Val (Boolean b)
    S.UnitVal -> return $ Val Unit
    S.Ref x -> case x of
      S.BoolVar n -> return $ Var (BoolVar n)
      S.NumVar n -> return $ Var (NumVar n)
      S.Array _ len addr -> Array <$> mapM (readHeap addr) [0 .. pred len]
    S.Add x y -> Add <$> simplifyM x <*> simplifyM y
    S.Sub x y -> Sub <$> simplifyM x <*> simplifyM y
    S.Mul x y -> Mul <$> simplifyM x <*> simplifyM y
    S.Div x y -> Div <$> simplifyM x <*> simplifyM y
    S.Eq x y -> Eq <$> simplifyM x <*> simplifyM y
    S.And x y -> And <$> simplifyM x <*> simplifyM y
    S.Or x y -> Or <$> simplifyM x <*> simplifyM y
    S.Xor x y -> Xor <$> simplifyM x <*> simplifyM y
    S.BEq x y -> BEq <$> simplifyM x <*> simplifyM y
    S.IfNum p x y -> If <$> simplifyM p <*> simplifyM x <*> simplifyM y
    S.IfBool p x y -> If <$> simplifyM p <*> simplifyM x <*> simplifyM y
    S.ToBool x -> ToBool <$> simplifyM x
    S.ToNum x -> ToNum <$> simplifyM x

instance Integral n => Simplify (S.Assignment 'S.Num n) Assignment where
  simplifyM (S.Assignment (S.NumVar n) e) = Assignment (NumVar n) <$> simplifyM e

instance Integral n => Simplify (S.Assignment 'S.Bool n) Assignment where
  simplifyM (S.Assignment (S.BoolVar n) e) = Assignment (BoolVar n) <$> simplifyM e
