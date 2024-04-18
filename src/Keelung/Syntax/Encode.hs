{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Module for encoding Keelung syntax
module Keelung.Syntax.Encode
  ( Encode (..),
    runHeapM,
    HeapM,
    encode',
    Expr (..),
  )
where

import Control.Monad.Reader
import Data.Array.Unboxed qualified as Array
import Data.IntMap qualified as IntMap
import GHC.Generics hiding (UInt)
import GHC.TypeLits (KnownNat)
import Keelung.Heap
import Keelung.Syntax (widthOf)
import Keelung.Syntax qualified as Syntax
import Keelung.Syntax.Encode.Syntax

--------------------------------------------------------------------------------

-- | MultiParam version of 'Encode'
class Encode' a b where
  -- | Encode a Keelung expression
  encode' :: a -> HeapM b

instance Encode' Syntax.Boolean Boolean where
  encode' expr = case expr of
    Syntax.Boolean b -> return $ ValB b
    Syntax.VarB var -> return $ VarB var
    Syntax.VarBI var -> return $ VarBI var
    Syntax.VarBP var -> return $ VarBP var
    Syntax.And x y -> AndB <$> encode' x <*> encode' y
    Syntax.Or x y -> OrB <$> encode' x <*> encode' y
    Syntax.Xor x y -> XorB <$> encode' x <*> encode' y
    Syntax.Not x -> NotB <$> encode' x
    Syntax.IfB p x y -> IfB <$> encode' p <*> encode' x <*> encode' y
    Syntax.EqB x y -> EqB <$> encode' x <*> encode' y
    Syntax.EqF x y -> EqF <$> encode' x <*> encode' y
    Syntax.EqU x y -> EqU (widthOf x) <$> encode' x <*> encode' y
    Syntax.LTU x y -> LTU (widthOf x) <$> encode' x <*> encode' y
    Syntax.LTEU x y -> LTEU (widthOf x) <$> encode' x <*> encode' y
    Syntax.GTU x y -> GTU (widthOf x) <$> encode' x <*> encode' y
    Syntax.GTEU x y -> GTEU (widthOf x) <$> encode' x <*> encode' y
    Syntax.BitU x i -> BitU (widthOf x) <$> encode' x <*> pure i

instance Encode' Syntax.Field Field where
  encode' expr = case expr of
    Syntax.Integer n -> return $ ValF n
    Syntax.Rational n -> return $ ValFR n
    Syntax.VarF var -> return $ VarF var
    Syntax.VarFI var -> return $ VarFI var
    Syntax.VarFP var -> return $ VarFP var
    Syntax.Add x y -> AddF <$> encode' x <*> encode' y
    Syntax.Sub x y -> SubF <$> encode' x <*> encode' y
    Syntax.Mul x y -> MulF <$> encode' x <*> encode' y
    Syntax.Exp x n -> ExpF <$> encode' x <*> pure n
    Syntax.Div x y -> DivF <$> encode' x <*> encode' y
    Syntax.IfF p x y -> IfF <$> encode' p <*> encode' x <*> encode' y
    Syntax.BtoF b -> BtoF <$> encode' b

instance (KnownNat w) => Encode' (Syntax.UInt w) UInt where
  encode' expr = case expr of
    Syntax.UInt n -> return $ ValU (widthOf expr) n
    Syntax.VarU var -> return $ VarU (widthOf expr) var
    Syntax.VarUI var -> return $ VarUI (widthOf expr) var
    Syntax.VarUP var -> return $ VarUP (widthOf expr) var
    Syntax.AddU x y -> AddU (widthOf x) <$> encode' x <*> encode' y
    Syntax.SubU x y -> SubU (widthOf x) <$> encode' x <*> encode' y
    Syntax.MulU x y -> MulU (widthOf expr) <$> encode' x <*> encode' y
    Syntax.MulD x y -> MulU (widthOf expr) <$> encode' x <*> encode' y
    Syntax.MulV x y -> MulU (widthOf expr) <$> encode' x <*> encode' y
    Syntax.AESMulU x y -> AESMulU (widthOf x) <$> encode' x <*> encode' y
    Syntax.CLMulU x y -> CLMulU (widthOf x) <$> encode' x <*> encode' y
    Syntax.MMIU x p -> MMIU (widthOf x) <$> encode' x <*> pure p
    Syntax.AndU x y -> AndU (widthOf expr) <$> encode' x <*> encode' y
    Syntax.OrU x y -> OrU (widthOf expr) <$> encode' x <*> encode' y
    Syntax.XorU x y -> XorU (widthOf expr) <$> encode' x <*> encode' y
    Syntax.NotU x -> NotU (widthOf expr) <$> encode' x
    Syntax.IfU p x y -> IfU (widthOf expr) <$> encode' p <*> encode' x <*> encode' y
    Syntax.RoLU w i x -> RoLU w i <$> encode' x
    Syntax.ShLU w i x -> ShLU w i <$> encode' x
    Syntax.SetU x i b -> SetU (widthOf expr) <$> encode' x <*> pure i <*> encode' b
    Syntax.BtoU n -> BtoU (widthOf expr) <$> encode' n
    Syntax.SliceU x i j -> SliceU (widthOf expr) <$> encode' x <*> pure i <*> pure j
    Syntax.JoinU x y -> JoinU (widthOf expr) <$> encode' x <*> encode' y

--------------------------------------------------------------------------------

-- | Typeclass for encoding stuff into something Keelung can understand
class Encode a where
  encode :: a -> HeapM Expr
  default encode :: (Generic a, GEncode (Rep a)) => a -> HeapM Expr
  encode a = gencode (from a)

instance Encode Syntax.Boolean where
  encode expr = Boolean <$> encode' expr

instance Encode Syntax.Field where
  encode expr = Field <$> encode' expr

instance (KnownNat w) => Encode (Syntax.UInt w) where
  encode expr = UInt <$> encode' expr

instance Encode () where
  encode expr = case expr of
    () -> return Unit

instance (Encode t) => Encode (ArrM t) where
  encode expr = case expr of
    ArrayRef _ len addr -> readArray addr len

instance (Encode t) => Encode [t] where
  encode xs = Array . Array.listArray (0, length xs - 1) <$> mapM encode xs

instance (Encode a, Encode b) => Encode (a, b) where
  encode (a, b) = do
    a' <- encode a
    b' <- encode b
    return $ Array $ Array.listArray (0, 1) [a', b']

-- | Generic Encode
class GEncode f where
  gencode :: f a -> HeapM Expr

instance GEncode U1 where
  gencode U1 = return Unit

instance (GEncode a, GEncode b) => GEncode (a :+: b) where
  gencode (L1 x) = gencode x
  gencode (R1 x) = gencode x

-- flatten all elements into 1-d array
instance (GEncode a, GEncode b) => GEncode (a :*: b) where
  gencode (a :*: b) = do
    a' <- gencode a
    b' <- gencode b
    return $ case (a', b') of
      (Array as, Array bs) ->
        let arr = Array.elems as ++ Array.elems bs
         in Array $ Array.listArray (0, length arr - 1) arr
      (Array as, _) -> Array $ Array.listArray (0, length as) (Array.elems as ++ [b'])
      (_, Array bs) -> Array $ Array.listArray (0, length bs) (a' : Array.elems bs)
      (_, _) -> Array $ Array.listArray (0, 1) [a', b']

instance (GEncode a) => GEncode (M1 i c a) where
  gencode (M1 x) = gencode x

instance (Encode a) => GEncode (K1 i a) where
  gencode (K1 x) = encode x

--------------------------------------------------------------------------------

-- | Reader Monad for Heap lookups
type HeapM = Reader Heap

-- | Run a HeapM computation
runHeapM :: Heap -> HeapM a -> a
runHeapM h m = runReader m h

-- | Read an array from the heap
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
            ElemF -> return $ Field $ VarF addr''
            ElemB -> return $ Boolean $ VarB addr''
            ElemU w -> return $ UInt $ VarU w addr''
            ElemArr _ len' -> readArray addr'' len'
            EmptyArr -> readArray addr'' 0
