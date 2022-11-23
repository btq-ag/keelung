{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module for converting Kinded syntax to Typed syntax
module Keelung.Syntax.Simplify (Elaborable (..), convert) where

import Control.Monad.Reader
import qualified Data.Array.Unboxed as Array
import qualified Data.IntMap as IntMap
import GHC.TypeLits (KnownNat)
import qualified Keelung.Monad as Kinded
import Keelung.Syntax.Kinded (widthOf)
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
            <$> mapM encodeAssignment asgns
            <*> mapM encodeAssignment bsgns
            <*> pure mempty
            <*> mapM convertM asgns'
        )

encodeAssignment :: Kinded.Assignment -> HeapM Assignment
encodeAssignment (Kinded.AssignmentN var e) = AssignmentN var <$> encode e
encodeAssignment (Kinded.AssignmentB var e) = AssignmentB var <$> encode e

--------------------------------------------------------------------------------

-- | Typeclass for encoding stuff into something Keelung can understand
class Encode a b where
  encode :: a -> HeapM b

instance Encode Kinded.Boolean Boolean where
  encode expr = case expr of
    Kinded.Boolean b -> return $ ValB b
    Kinded.VarB var -> return $ VarB var
    Kinded.InputVarB var -> return $ InputVarB var
    Kinded.And x y -> AndB <$> encode x <*> encode y
    Kinded.Or x y -> OrB <$> encode x <*> encode y
    Kinded.Xor x y -> XorB <$> encode x <*> encode y
    Kinded.IfB p x y -> IfB <$> encode p <*> encode x <*> encode y
    Kinded.EqB x y -> EqB <$> encode x <*> encode y
    Kinded.EqN x y -> EqN <$> encode x <*> encode y
    Kinded.EqU x y -> EqU (widthOf x) <$> encode x <*> encode y
    others -> LoopholeB <$> convertM others

instance Encode Kinded.Number Number where
  encode expr = case expr of
    Kinded.Integer n -> return $ ValN n
    Kinded.Rational n -> return $ ValNR n
    Kinded.VarN var -> return $ VarN var
    Kinded.InputVarN var -> return $ InputVarN var
    Kinded.Add x y -> AddN <$> encode x <*> encode y
    Kinded.Sub x y -> SubN <$> encode x <*> encode y
    Kinded.Mul x y -> MulN <$> encode x <*> encode y
    Kinded.Div x y -> DivN <$> encode x <*> encode y
    Kinded.IfN p x y -> IfN <$> encode p <*> encode x <*> encode y
    others -> LoopholeN <$> convertM others

instance KnownNat w => Encode (Kinded.UInt w) UInt where
  encode expr = case expr of
    Kinded.UInt n -> return $ ValU (widthOf expr) n
    Kinded.VarU var -> return $ VarU (widthOf expr) var
    Kinded.InputVarU var -> return $ InputVarU (widthOf expr) var
    Kinded.AddU x y -> AddU (widthOf x) <$> encode x <*> encode y
    Kinded.SubU x y -> SubU (widthOf x) <$> encode x <*> encode y
    Kinded.MulU x y -> MulU (widthOf x) <$> encode x <*> encode y
    Kinded.AndU x y -> AndU (widthOf expr) <$> encode x <*> encode y
    Kinded.OrU x y -> OrU (widthOf expr) <$> encode x <*> encode y
    Kinded.XorU x y -> XorU (widthOf expr) <$> encode x <*> encode y
    Kinded.NotU x -> NotU (widthOf expr) <$> encode x
    Kinded.IfU p x y -> IfU (widthOf expr) <$> encode p <*> encode x <*> encode y
    others -> LoopholeU (widthOf expr) <$> convertM others

--------------------------------------------------------------------------------

-- | Typeclass for removing kinds
class Elaborable a where
  convertM :: a -> HeapM Expr

instance Elaborable Kinded.Number where
  convertM expr = case expr of
    Kinded.Integer n -> return $ Number (ValN n)
    Kinded.Rational n -> return $ Number (ValNR n)
    Kinded.VarN var -> return $ Number (VarN var)
    Kinded.InputVarN var -> return $ Number (InputVarN var)
    Kinded.Add x y -> Number <$> (AddN <$> encode x <*> encode y)
    Kinded.Sub x y -> Number <$> (SubN <$> encode x <*> encode y)
    Kinded.Mul x y -> Number <$> (MulN <$> encode x <*> encode y)
    Kinded.Div x y -> Number <$> (DivN <$> encode x <*> encode y)
    Kinded.IfN p x y -> Number <$> (IfN <$> encode p <*> encode x <*> encode y)
    Kinded.FromBool x -> ToNum <$> convertM x
    Kinded.FromUInt x -> ToNum <$> convertM x

instance KnownNat w => Elaborable (Kinded.UInt w) where
  convertM expr = case expr of
    Kinded.UInt n -> return $ UInt (ValU (widthOf expr) n)
    Kinded.VarU n -> return $ UInt (VarU (widthOf expr) n)
    Kinded.InputVarU n -> return $ UInt (InputVarU (widthOf expr) n)
    Kinded.AddU x y -> UInt <$> (AddU (widthOf expr) <$> encode x <*> encode y)
    Kinded.SubU x y -> UInt <$> (SubU (widthOf expr) <$> encode x <*> encode y)
    Kinded.MulU x y -> UInt <$> (MulU (widthOf expr) <$> encode x <*> encode y)
    Kinded.AndU x y -> UInt <$> (AndU (widthOf expr) <$> encode x <*> encode y)
    Kinded.OrU x y -> UInt <$> (OrU (widthOf expr) <$> encode x <*> encode y)
    Kinded.XorU x y -> UInt <$> (XorU (widthOf expr) <$> encode x <*> encode y)
    Kinded.NotU x -> UInt . NotU (widthOf expr) <$> encode x
    Kinded.RoLU n x -> UInt <$> (RoLU (widthOf expr) n <$> encode x)
    Kinded.IfU p x y -> UInt <$> (IfU (widthOf expr) <$> encode p <*> encode x <*> encode y)
    Kinded.ToUInt x -> ToNum <$> convertM x

instance Elaborable Kinded.Boolean where
  convertM expr = case expr of
    Kinded.Boolean b -> return $ Boolean (ValB b)
    Kinded.VarB var -> return $ Boolean (VarB var)
    Kinded.InputVarB var -> return $ Boolean (InputVarB var)
    Kinded.NumBit n i -> Bit <$> convertM n <*> return i
    Kinded.UIntBit n i -> Bit <$> convertM n <*> return i
    Kinded.EqB x y -> Boolean <$> (EqB <$> encode x <*> encode y)
    Kinded.EqN x y -> Boolean <$> (EqN <$> encode x <*> encode y)
    Kinded.EqU x y -> Boolean <$> (EqU (widthOf x) <$> encode x <*> encode y)
    Kinded.And x y -> Boolean <$> (AndB <$> encode x <*> encode y)
    Kinded.Or x y -> Boolean <$> (OrB <$> encode x <*> encode y)
    Kinded.Xor x y -> Boolean <$> (XorB <$> encode x <*> encode y)
    -- Kinded.EqU x y -> EqB <$> convertM x <*> convertM y
    Kinded.IfB p x y -> Boolean <$> (IfB <$> encode p <*> encode x <*> encode y)

instance Elaborable () where
  convertM expr = case expr of
    () -> return Unit

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
            Kinded.NumElem -> return $ Number $ VarN addr''
            Kinded.BoolElem -> return $ Boolean $ VarB addr''
            Kinded.ArrElem _ len' -> readArray addr'' len'
