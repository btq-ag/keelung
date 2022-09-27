-- | Module for converting Kinded syntax to Typed syntax
module Keelung.Syntax.Simplify (Simplify, simplify, simplifyM, simplifyComputation) where

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
      <$> mapM simplifyAssignment asgns
      <*> mapM simplifyAssignment bsgns
      <*> mapM simplifyM asgns'

simplifyAssignment :: Kinded.Assignment -> HeapM Assignment
simplifyAssignment (Kinded.BoolAssignment var e) = Assignment (NumVar var) <$> simplifyM e
simplifyAssignment (Kinded.NumAssignment var e) = Assignment (BoolVar var) <$> simplifyM e

simplify :: Simplify t => Kinded.Elaborated t -> Elaborated
simplify (Kinded.Elaborated expr comp) =
  let comp' = simplifyComputation comp
   in Elaborated
        (runHeapM (compHeap comp') (simplifyM expr))
        comp'

--------------------------------------------------------------------------------

-- | Typeclass for removing kinds
class Simplify a where
  simplifyM :: a -> HeapM Expr

instance Simplify Kinded.Number where
  simplifyM expr = case expr of
    Kinded.Integer n -> return $ Val (Integer n)
    Kinded.Rational n -> return $ Val (Rational n)
    Kinded.NumVar var -> return $ Var (NumVar var)
    Kinded.NumInputVar var -> return $ Var (NumInputVar var)
    Kinded.Add x y -> Add <$> simplifyM x <*> simplifyM y
    Kinded.Sub x y -> Sub <$> simplifyM x <*> simplifyM y
    Kinded.Mul x y -> Mul <$> simplifyM x <*> simplifyM y
    Kinded.Div x y -> Div <$> simplifyM x <*> simplifyM y
    Kinded.IfNum p x y -> If <$> simplifyM p <*> simplifyM x <*> simplifyM y
    Kinded.ToNum x -> ToNum <$> simplifyM x

instance Simplify Kinded.Boolean where
  simplifyM expr = case expr of
    Kinded.Boolean b -> return $ Val (Boolean b)
    Kinded.BoolVar var -> return $ Var (BoolVar var)
    Kinded.BoolInputVar var -> return $ Var (BoolInputVar var)
    Kinded.Eq x y -> Eq <$> simplifyM x <*> simplifyM y
    Kinded.And x y -> And <$> simplifyM x <*> simplifyM y
    Kinded.Or x y -> Or <$> simplifyM x <*> simplifyM y
    Kinded.Xor x y -> Xor <$> simplifyM x <*> simplifyM y
    Kinded.BEq x y -> BEq <$> simplifyM x <*> simplifyM y
    Kinded.IfBool p x y -> If <$> simplifyM p <*> simplifyM x <*> simplifyM y
    Kinded.ToBool x -> ToBool <$> simplifyM x

instance Simplify Kinded.Unit where
  simplifyM expr = case expr of
    Kinded.Unit -> return $ Val Unit

instance Simplify t => Simplify (Kinded.Arr t) where
  simplifyM expr = case expr of
    Kinded.Arr xs -> Array <$> mapM simplifyM xs

instance Simplify t => Simplify (Kinded.ArrM t) where
  simplifyM expr = case expr of
    Kinded.ArrayRef _ len addr -> readArray addr len

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
