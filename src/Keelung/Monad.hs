{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Keelung.Monad
  ( Comp,
    runComp,
    Computation (..),
    emptyComputation,
    Elaborated (..),
    Assignment (..),

    -- * Array
    Referable (),
    toArray,
    toArrayI,
    toArrayI',
    fromArray,
    lengthOf,
    update,
    access,
    access2,
    access3,

    -- * Inputs
    input,
    inputNum,
    inputBool,
    inputs,
    inputs2,
    inputs3,

    -- * Statements
    cond,
    assert,
    reduce,
  )
where

import Control.Monad.Except
import Control.Monad.State.Strict hiding (get, put)
import Data.Array ((!))
import Data.Array.IArray (Array)
import qualified Data.Array.IArray as IArray
import Data.Field.Galois (GaloisField)
import Data.Foldable (toList)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Keelung.Error
import Keelung.Field
import Keelung.Syntax
import Keelung.Types
import Prelude hiding (product, sum)

--------------------------------------------------------------------------------

-- | An Assignment associates an expression with a reference
data Assignment t n = Assignment (Ref t) (Val t n)
  deriving (Eq)

instance Show n => Show (Assignment t n) where
  show (Assignment var expr) = show var <> " := " <> show expr

instance Functor (Assignment t) where
  fmap f (Assignment var expr) = Assignment var (fmap f expr)

--------------------------------------------------------------------------------

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
    compNumAsgns :: [Assignment 'Num n],
    compBoolAsgns :: [Assignment 'Bool n],
    -- Assertions are expressions that are expected to be true
    compAssertions :: [Val 'Bool n]
  }
  deriving (Eq)

emptyComputation :: Computation n
emptyComputation = Computation 0 0 mempty mempty mempty mempty mempty

instance (Show n, GaloisField n, Bounded n, Integral n) => Show (Computation n) where
  show (Computation nextVar nextAddr inputVars _ numAsgns boolAsgns assertions) =
    "{\n  variable counter: " ++ show nextVar
      ++ "\n  address counter: "
      ++ show nextAddr
      ++ "\n  input variables: "
      ++ show (IntSet.toList inputVars)
      ++ "\n  num assignments: "
      ++ show (map (fmap N) numAsgns)
      ++ "\n  bool assignments: "
      ++ show (map (fmap N) boolAsgns)
      ++ "\n  assertions: "
      ++ show (map (fmap N) assertions)
      ++ "\n\
         \}"

--------------------------------------------------------------------------------

-- | The result of elaborating a computation
data Elaborated t n = Elaborated
  { -- | The resulting 'Expr'
    elabVal :: !(Val t n),
    -- | The state of computation after elaboration
    elabComp :: Computation n
  }
  deriving (Eq)

instance (Show n, GaloisField n, Bounded n, Integral n) => Show (Elaborated t n) where
  show (Elaborated expr comp) =
    "{\n expression: "
      ++ show (fmap N expr)
      ++ "\n  compuation state: \n"
      ++ show comp
      ++ "\n}"

--------------------------------------------------------------------------------

-- | The type of a Keelung program
type Comp n = StateT (Computation n) (Except ElabError)

-- | How to run the 'Comp' monad
runComp :: Computation n -> Comp n a -> Either ElabError (a, Computation n)
runComp comp f = runExcept (runStateT f comp)

--------------------------------------------------------------------------------
-- Variable & Input Variable
--------------------------------------------------------------------------------

-- | Allocate a fresh address.
freshVar :: Comp n Var
freshVar = do
  index <- gets compNextVar
  modify (\st -> st {compNextVar = succ index})
  return index

freshAddr :: Comp n Addr
freshAddr = do
  addr <- gets compNextAddr
  modify (\st -> st {compNextAddr = succ addr})
  return addr

--------------------------------------------------------------------------------

-- | Update an entry of an array.
-- When the assigned expression is a variable,
-- we update the entry directly with the variable instead
update :: Referable t => Val ('Arr t) n -> Int -> Val t n -> Comp n ()
update (ArrayVal _) _ _ = return () -- No-op since ArrayVal is immutable
update (Ref (ArrayRef _ _ addr)) i (Ref (NumVar n)) = writeHeap addr NumElem (i, n)
update (Ref (ArrayRef _ _ addr)) i (Ref (BoolVar n)) = writeHeap addr BoolElem (i, n)
update (Ref (ArrayRef elemType _ addr)) i expr = do
  ref <- alloc expr
  writeHeap addr elemType (i, addrOfRef ref)

-- | Typeclass for operations on base types
class Proper t where
  -- | Request a fresh input
  input :: Comp n (Val t n)

  -- | Conditional clause
  cond :: Val 'Bool n -> Val t n -> Val t n -> Val t n

instance Proper 'Num where
  input = inputNum
  cond = IfNum

instance Proper 'Bool where
  input = inputBool
  cond = IfBool

-- | Requests a fresh Num input variable
inputNum :: Comp n (Val 'Num n)
inputNum = do
  var <- freshVar
  markVarAsInput var
  return $ Ref $ NumVar var

-- | Requests a fresh Bool input variable
inputBool :: Comp n (Val 'Bool n)
inputBool = do
  var <- freshVar
  markVarAsInput var
  return $ Ref $ BoolVar var

--------------------------------------------------------------------------------
-- Array & Input Array
--------------------------------------------------------------------------------

-- | Converts a list of values to an 1D-array
toArray :: Referable t => [Val t n] -> Comp n (Val ('Arr t) n)
toArray xs = do
  when (null xs) $ throwError EmptyArrayError
  let kind = typeOf (head xs)
  Ref <$> allocArray kind xs

-- | Immutable version of `toArray`
toArrayI :: Referable t => [Val t n] -> Val ('Arr t) n
toArrayI xs = ArrayVal $ IArray.listArray (0, length xs - 1) xs

-- | Immutable version of `toArray`
toArrayI' :: Referable t => Array Int (Val t n) -> Val ('Arr t) n
toArrayI' = ArrayVal

-- | Convert an array into a list of expressions
fromArray :: Referable t => Val ('Arr t) n -> Comp n [Val t n]
fromArray (ArrayVal xs) = return $ toList xs
fromArray (Ref (ArrayRef _ len addr)) = do
  -- collect addresses or variables of each element
  forM [0 .. pred len] $ \i -> do
    Ref <$> readHeap (addr, i)

--------------------------------------------------------------------------------

-- | Requests a 1D-array of fresh input variables
inputs :: (Proper t, Referable t) => Int -> Comp n (Val ('Arr t) n)
inputs 0 = throwError EmptyArrayError
inputs size = do
  vars <- replicateM size input
  toArray vars

-- | Requests a 2D-array of fresh input variables
inputs2 :: (Proper t, Referable t) => Int -> Int -> Comp n (Val ('Arr ('Arr t)) n)
inputs2 0 _ = throwError EmptyArrayError
inputs2 _ 0 = throwError EmptyArrayError
inputs2 sizeM sizeN = do
  vars <- replicateM sizeM (inputs sizeN)
  toArray vars

-- | Requests a 3D-array of fresh input variables
inputs3 :: (Proper t, Referable t) => Int -> Int -> Int -> Comp n (Val ('Arr ('Arr ('Arr t))) n)
inputs3 0 _ _ = throwError EmptyArrayError
inputs3 _ 0 _ = throwError EmptyArrayError
inputs3 _ _ 0 = throwError EmptyArrayError
inputs3 sizeM sizeN sizeO = do
  vars <- replicateM sizeM (inputs2 sizeN sizeO)
  toArray vars

--------------------------------------------------------------------------------

-- | Typeclass for retrieving the element of an array
class Referable t where
  -- | Allocates a fresh variable for a value
  alloc :: Val t n -> Comp n (Ref t)

  typeOf :: Val t n -> ElemType

  constructElementRef :: ElemType -> Addr -> Ref t

instance Referable ref => Referable ('Arr ref) where
  alloc (ArrayVal xs) = do
    when (null xs) $ throwError EmptyArrayError
    allocArray (typeOf (xs ! 0)) (toList xs)
  alloc xs@(Ref (ArrayRef elemType len _)) = do
    elements <- mapM (access xs) [0 .. len - 1]
    allocArray elemType elements

  typeOf (ArrayVal xs) = typeOf (xs ! 0)
  typeOf (Ref (ArrayRef elemType len _)) = ArrElem elemType len

  constructElementRef (ArrElem l k) elemAddr = ArrayRef l k elemAddr
  constructElementRef _ _ = error "expecting element to be array"

instance Referable 'Num where
  alloc val = do
    var <- freshVar
    modify' $ \st -> st {compNumAsgns = Assignment (NumVar var) val : compNumAsgns st}
    return $ NumVar var

  typeOf _ = NumElem

  constructElementRef NumElem elemAddr = NumVar elemAddr
  constructElementRef _ _ = error "expecting element to be of Num"

instance Referable 'Bool where
  alloc val = do
    var <- freshVar
    modify' $ \st -> st {compBoolAsgns = Assignment (BoolVar var) val : compBoolAsgns st}
    return $ BoolVar var

  typeOf _ = BoolElem

  constructElementRef BoolElem elemAddr = BoolVar elemAddr
  constructElementRef _ _ = error "expecting element to be of Bool"

-- | Access an element from a 1-D array
access :: Referable t => Val ('Arr t) n -> Int -> Comp n (Val t n)
access (ArrayVal xs) i =
  if i < length xs
    then return $ xs ! i
    else throwError $ IndexOutOfBoundsError2 (length xs) i
access (Ref (ArrayRef _ _ addr)) i = Ref <$> readHeap (addr, i)

-- | Access an element from a 2-D array
access2 :: Referable t => Val ('Arr ('Arr t)) n -> (Int, Int) -> Comp n (Val t n)
access2 addr (i, j) = access addr i >>= flip access j

-- | Access an element from a 3-D array
access3 :: Referable t => Val ('Arr ('Arr ('Arr t))) n -> (Int, Int, Int) -> Comp n (Val t n)
access3 addr (i, j, k) = access addr i >>= flip access j >>= flip access k

--------------------------------------------------------------------------------

-- | Internal helper function extracting the address of a reference
addrOfRef :: Ref t -> Addr
addrOfRef (BoolVar addr) = addr
addrOfRef (NumVar addr) = addr
addrOfRef (ArrayRef _ _ addr) = addr

-- | Internal helper function for allocating an array with values
allocArray :: Referable t => ElemType -> [Val t n] -> Comp n (Ref ('Arr ty))
allocArray elemType vals = do
  -- allocate new variables for each element
  refs <- mapM alloc vals
  -- allocate new array for holding the variables of these elements
  addr <- allocOnHeap elemType refs
  return $ ArrayRef elemType (length refs) addr

-- | Internal helper function for marking a variable as input.
markVarAsInput :: Var -> Comp n ()
markVarAsInput = markVarsAsInput . IntSet.singleton

-- | Internal helper function for marking multiple variables as input
markVarsAsInput :: IntSet -> Comp n ()
markVarsAsInput vars =
  modify (\st -> st {compInputVars = vars <> compInputVars st})

-- | Internal helper function for allocating an array on the heap
allocOnHeap :: ElemType -> [Ref t] -> Comp n Addr
allocOnHeap elemType refs = do
  addr <- freshAddr
  let addresses = map addrOfRef refs
  let bindings = IntMap.fromDistinctAscList $ zip [0 ..] addresses
  heap <- gets compHeap
  let heap' = IntMap.insert addr (elemType, bindings) heap
  modify (\st -> st {compHeap = heap'})
  return addr

-- | Internal helper function for updating an array entry on the heap
writeHeap :: Addr -> ElemType -> (Int, Var) -> Comp n ()
writeHeap addr elemType (index, var) = do
  let bindings = IntMap.singleton index var
  heap <- gets compHeap
  let heap' = IntMap.insertWith (<>) addr (elemType, bindings) heap
  modify (\st -> st {compHeap = heap'})

-- | Internal helper function for access an array on the heap
readHeap :: Referable t => (Addr, Int) -> Comp n (Ref t)
readHeap (addr, i) = do
  heap <- gets compHeap
  case IntMap.lookup addr heap of
    Nothing -> error "readHeap: address not found"
    Just (elemType, array) -> case IntMap.lookup i array of
      Nothing -> throwError $ IndexOutOfBoundsError addr i array
      Just n -> return $ constructElementRef elemType n

--------------------------------------------------------------------------------

-- | An alternative to 'foldM'
reduce :: Foldable m => Val t n -> m a -> (Val t n -> a -> Comp n (Val t n)) -> Comp n (Val t n)
reduce a xs f = foldM f a xs

lengthOf :: Val ('Arr t) n -> Int
lengthOf (ArrayVal xs) = length xs
lengthOf (Ref (ArrayRef _ len _)) = len

--------------------------------------------------------------------------------

-- | Assert that the given expression is true
assert :: Val 'Bool n -> Comp n ()
assert expr = modify' $ \st -> st {compAssertions = expr : compAssertions st}
