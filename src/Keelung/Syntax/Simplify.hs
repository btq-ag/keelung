-- | Module for converting Kinded syntax to Typed syntax
module Keelung.Syntax.Simplify (Elaborable (..), convert) where

import Control.Monad.Reader
import qualified Data.Array.Unboxed as Array
import qualified Data.IntMap as IntMap
import qualified Keelung.Monad as Kinded
import qualified Keelung.Syntax.Kinded as Kinded
import Keelung.Syntax.Typed
import Keelung.Types (Addr, Heap)
import qualified Keelung.Types as Kinded

--------------------------------------------------------------------------------

convert :: Elaborable t => Kinded.Elaborated t -> Elaborated
convert (Kinded.Elaborated expr comp) = runHeapM (Kinded.compHeap comp) $ do
  let Kinded.Computation varCounters _addrSize _heap asgns bsgns asgns' = comp
  Elaborated
    <$> convertM expr
    <*> ( Computation
            varCounters
            <$> mapM convertAssignment asgns
            <*> mapM convertAssignment bsgns
            <*> mapM convertM asgns'
        )

convertAssignment :: Kinded.Assignment -> HeapM Assignment
convertAssignment (Kinded.BoolAssignment var e) = Assignment (NumVar var) <$> convertM e
convertAssignment (Kinded.NumAssignment var e) = Assignment (BoolVar var) <$> convertM e

--------------------------------------------------------------------------------

-- | Typeclass for removing kinds
class Elaborable a where
  convertM :: a -> HeapM Expr

instance Elaborable Kinded.Number where
  convertM expr = case expr of
    Kinded.Integer n -> return $ Val (Integer n)
    Kinded.Rational n -> return $ Val (Rational n)
    Kinded.NumVar var -> return $ Var (NumVar var)
    Kinded.NumInputVar var -> return $ Var (NumInputVar var)
    Kinded.Add x y -> Add <$> convertM x <*> convertM y
    Kinded.Sub x y -> Sub <$> convertM x <*> convertM y
    Kinded.Mul x y -> Mul <$> convertM x <*> convertM y
    Kinded.Div x y -> Div <$> convertM x <*> convertM y
    Kinded.IfNum p x y -> If <$> convertM p <*> convertM x <*> convertM y
    Kinded.ToNum x -> ToNum <$> convertM x

instance Elaborable Kinded.Boolean where
  convertM expr = case expr of
    Kinded.Boolean b -> return $ Val (Boolean b)
    Kinded.BoolVar var -> return $ Var (BoolVar var)
    Kinded.BoolInputVar var -> return $ Var (BoolInputVar var)
    Kinded.NumBit n i -> Bit <$> convertM n <*> return i
    Kinded.Eq x y -> Eq <$> convertM x <*> convertM y
    Kinded.And x y -> And <$> convertM x <*> convertM y
    Kinded.Or x y -> Or <$> convertM x <*> convertM y
    Kinded.Xor x y -> Xor <$> convertM x <*> convertM y
    Kinded.BEq x y -> BEq <$> convertM x <*> convertM y
    Kinded.IfBool p x y -> If <$> convertM p <*> convertM x <*> convertM y

instance Elaborable () where
  convertM expr = case expr of
    () -> return $ Val Unit

instance Elaborable t => Elaborable (Kinded.Arr t) where
  convertM expr = case expr of
    Kinded.Arr xs -> Array <$> mapM convertM xs

instance Elaborable t => Elaborable (Kinded.ArrM t) where
  convertM expr = case expr of
    Kinded.ArrayRef _ len addr -> readArray addr len

--------------------------------------------------------------------------------

-- | Reader Monad for Heap lookups
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
