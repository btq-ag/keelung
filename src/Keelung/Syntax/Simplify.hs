{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Module for converting Kinded syntax to Typed syntax
module Keelung.Syntax.Simplify (simplify, simplifyM, simplifyComputation) where

import Control.Monad.Reader
import qualified Data.Array.Unboxed as Array
import qualified Data.IntMap as IntMap
import Data.Proxy (Proxy (Proxy))
import Keelung.Field (AcceptedField, encodeFieldType)
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
readArray addr len = Array <$> mapM (readHeap addr) (Array.listArray (0, len) [0 .. pred len])
  where
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

simplifyComputation :: (AcceptedField n, Integral n) => Kinded.Computation n -> Computation
simplifyComputation (Kinded.Computation nextVar nextAddr inputVars heap asgns bsgns asgns') =
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
    toProxy :: [Kinded.Val kind n] -> Proxy n
    toProxy = const Proxy

simplify :: (Integral n, AcceptedField n) => Kinded.Elaborated t n -> Elaborated
simplify (Kinded.Elaborated expr comp) =
  let comp' = simplifyComputation comp
   in Elaborated
        (runHeapM (compHeap comp') (simplifyM expr))
        comp'

--------------------------------------------------------------------------------

-- | Typeclass for removing kinds
class Simplify a b where
  simplifyM :: a -> HeapM b

instance Integral n => Simplify (Kinded.Val t n) Expr where
  simplifyM expr = case expr of
    Kinded.Integer n -> return $ Val (Integer (toInteger n))
    Kinded.Rational n -> return $ Val (Rational n)
    Kinded.Boolean b -> return $ Val (Boolean b)
    Kinded.UnitVal -> return $ Val Unit
    Kinded.ArrayVal xs -> Array <$> mapM simplifyM xs
    Kinded.Ref x -> case x of
      Kinded.BoolVar n -> return $ Var (BoolVar n)
      Kinded.NumVar n -> return $ Var (NumVar n)
      Kinded.ArrayRef _ len addr -> readArray addr len
    Kinded.Add x y -> Add <$> simplifyM x <*> simplifyM y
    Kinded.Sub x y -> Sub <$> simplifyM x <*> simplifyM y
    Kinded.Mul x y -> Mul <$> simplifyM x <*> simplifyM y
    Kinded.Div x y -> Div <$> simplifyM x <*> simplifyM y
    Kinded.Eq x y -> Eq <$> simplifyM x <*> simplifyM y
    Kinded.And x y -> And <$> simplifyM x <*> simplifyM y
    Kinded.Or x y -> Or <$> simplifyM x <*> simplifyM y
    Kinded.Xor x y -> Xor <$> simplifyM x <*> simplifyM y
    Kinded.BEq x y -> BEq <$> simplifyM x <*> simplifyM y
    Kinded.IfNum p x y -> If <$> simplifyM p <*> simplifyM x <*> simplifyM y
    Kinded.IfBool p x y -> If <$> simplifyM p <*> simplifyM x <*> simplifyM y
    Kinded.ToBool x -> ToBool <$> simplifyM x
    Kinded.ToNum x -> ToNum <$> simplifyM x

instance Integral n => Simplify (Kinded.Assignment 'Kinded.Num n) Assignment where
  simplifyM (Kinded.Assignment (Kinded.NumVar n) e) = Assignment (NumVar n) <$> simplifyM e

instance Integral n => Simplify (Kinded.Assignment 'Kinded.Bool n) Assignment where
  simplifyM (Kinded.Assignment (Kinded.BoolVar n) e) = Assignment (BoolVar n) <$> simplifyM e
