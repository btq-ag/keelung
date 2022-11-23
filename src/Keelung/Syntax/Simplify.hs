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
            <*> pure mempty
            <*> mapM convertM asgns'
        )

convertAssignment :: Kinded.Assignment -> HeapM Assignment
convertAssignment (Kinded.BoolAssignment var e) = Assignment (VarN var) <$> convertM e
convertAssignment (Kinded.NumAssignment var e) = Assignment (VarB var) <$> convertM e

--------------------------------------------------------------------------------

-- | Typeclass for removing kinds
class Elaborable a where
  convertM :: a -> HeapM Expr

instance Elaborable Kinded.Number where
  convertM expr = case expr of
    Kinded.Integer n -> return $ Val (Integer n)
    Kinded.Rational n -> return $ Val (Rational n)
    Kinded.VarN var -> return $ Var (VarN var)
    Kinded.InputVarN var -> return $ Var (InputVarN var)
    Kinded.Add x y -> Add <$> convertM x <*> convertM y
    Kinded.Sub x y -> Sub <$> convertM x <*> convertM y
    Kinded.Mul x y -> Mul <$> convertM x <*> convertM y
    Kinded.Div x y -> Div <$> convertM x <*> convertM y
    -- Kinded.AndN x y -> And <$> convertM x <*> convertM y
    -- Kinded.OrN x y -> Or <$> convertM x <*> convertM y
    -- Kinded.XorN x y -> Xor <$> convertM x <*> convertM y
    -- Kinded.RoRN n x -> RotateR n <$> convertM x
    Kinded.IfN p x y -> If <$> convertM p <*> convertM x <*> convertM y
    Kinded.FromBool x -> ToNum <$> convertM x
    Kinded.FromUInt x -> ToNum <$> convertM x

instance Elaborable (Kinded.UInt w) where
  convertM expr = case expr of
    Kinded.UInt w n -> return $ Val (Unsigned w n)
    Kinded.VarU w n -> return $ Var (VarU w n)
    Kinded.InputVarU w n -> return $ Var (InputVarU w n)
    Kinded.AddU x y -> Add <$> convertM x <*> convertM y
    Kinded.SubU x y -> Sub <$> convertM x <*> convertM y
    Kinded.MulU x y -> Mul <$> convertM x <*> convertM y
    Kinded.AndU x y -> And <$> convertM x <*> convertM y
    Kinded.OrU x y -> Or <$> convertM x <*> convertM y
    Kinded.XorU x y -> Xor <$> convertM x <*> convertM y
    Kinded.NotU x -> NotU <$> convertM x
    Kinded.RoRU n x -> RotateR n <$> convertM x
    Kinded.IfU p x y -> If <$> convertM p <*> convertM x <*> convertM y
    Kinded.ToUInt x -> ToNum <$> convertM x

instance Elaborable Kinded.Boolean where
  convertM expr = case expr of
    Kinded.Boolean b -> return $ Val (Boolean b)
    Kinded.VarB var -> return $ Var (VarB var)
    Kinded.InputVarB var -> return $ Var (InputVarB var)
    Kinded.NumBit n i -> Bit <$> convertM n <*> return i
    Kinded.UIntBit n i -> Bit <$> convertM n <*> return i
    Kinded.Eq x y -> Eq <$> convertM x <*> convertM y
    Kinded.And x y -> And <$> convertM x <*> convertM y
    Kinded.Or x y -> Or <$> convertM x <*> convertM y
    Kinded.Xor x y -> Xor <$> convertM x <*> convertM y
    Kinded.BEq x y -> BEq <$> convertM x <*> convertM y
    Kinded.UEq x y -> BEq <$> convertM x <*> convertM y
    Kinded.IfB p x y -> If <$> convertM p <*> convertM x <*> convertM y

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
            Kinded.NumElem -> return $ Var $ VarN addr''
            Kinded.BoolElem -> return $ Var $ VarB addr''
            Kinded.ArrElem _ len' -> readArray addr'' len'
