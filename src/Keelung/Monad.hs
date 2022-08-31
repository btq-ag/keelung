{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
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
    Mutable (),
    toArrayM,
    toArray,
    toArray',
    fromArray,
    fromArrayM,
    lengthOf,
    lengthOfM,
    updateM,
    accessM,
    accessM2,
    accessM3,
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
import Data.Array.Unboxed (Array)
import qualified Data.Array.Unboxed as IArray
import Data.Foldable (toList)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Keelung.Error
import Keelung.Syntax
import Keelung.Types
import Prelude hiding (product, sum)

--------------------------------------------------------------------------------

-- | An Assignment associates an expression with a reference
data Assignment t n = Assignment (Ref t) (Val t n)
  deriving (Eq)

instance Show (Assignment t n) where
  show (Assignment var expr) = show var <> " := " <> show expr

-- instance Functor (Assignment t) where
--   fmap f (Assignment var expr) = Assignment var (fmap f expr)

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

instance Show (Computation n) where
  show (Computation nextVar nextAddr inputVars _ numAsgns boolAsgns assertions) =
    "{\n  variable counter: " ++ show nextVar
      ++ "\n  address counter: "
      ++ show nextAddr
      ++ "\n  input variables: "
      ++ show (IntSet.toList inputVars)
      ++ "\n  num assignments: "
      ++ show numAsgns
      ++ "\n  bool assignments: "
      ++ show boolAsgns
      ++ "\n  assertions: "
      ++ show assertions
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

instance Show (Elaborated t n) where
  show (Elaborated expr comp) =
    "{\n expression: "
      ++ show expr
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
updateM :: Mutable t => Val ('ArrM t) n -> Int -> Val t n -> Comp n ()
updateM (Ref (ArrayRef _ _ addr)) i (Ref (NumVar n)) = writeHeap addr NumElem (i, n)
updateM (Ref (ArrayRef _ _ addr)) i (Ref (BoolVar n)) = writeHeap addr BoolElem (i, n)
updateM (Ref (ArrayRef elemType _ addr)) i expr = do
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
toArrayM :: Mutable t => [Val t n] -> Comp n (Val ('ArrM t) n)
toArrayM xs = do
  when (null xs) $ throwError EmptyArrayError
  let kind = typeOf (head xs)
  Ref <$> allocArray kind xs

-- | Immutable version of `toArray`
toArray :: [Val t n] -> Val ('Arr t) n
toArray xs = ArrayVal $ IArray.listArray (0, length xs - 1) xs

-- | Immutable version of `toArray`
toArray' :: Array Int (Val t n) -> Val ('Arr t) n
toArray' = ArrayVal

-- | Convert an array into a list of expressions
fromArrayM :: Mutable t => Val ('ArrM t) n -> Comp n [Val t n]
fromArrayM (Ref (ArrayRef _ _ addr)) = do
  -- collect references of each element
  refs <- readHeapArray addr
  return $ map Ref refs

fromArray :: Val ('Arr t) n -> [Val t n]
fromArray (ArrayVal xs) = toList xs
fromArray (Ref ref) = case ref of {}

--------------------------------------------------------------------------------

-- | Requests a 1D-array of fresh input variables
inputs :: (Proper t, Mutable t) => Int -> Comp n (Val ('Arr t) n)
inputs 0 = throwError EmptyArrayError
inputs size = do
  vars <- replicateM size input
  return $ toArray vars

-- | Requests a 2D-array of fresh input variables
inputs2 :: (Proper t, Mutable t) => Int -> Int -> Comp n (Val ('Arr ('Arr t)) n)
inputs2 0 _ = throwError EmptyArrayError
inputs2 _ 0 = throwError EmptyArrayError
inputs2 sizeM sizeN = do
  vars <- replicateM sizeM (inputs sizeN)
  return $ toArray vars

-- | Requests a 3D-array of fresh input variables
inputs3 :: (Proper t, Mutable t) => Int -> Int -> Int -> Comp n (Val ('Arr ('Arr ('Arr t))) n)
inputs3 0 _ _ = throwError EmptyArrayError
inputs3 _ 0 _ = throwError EmptyArrayError
inputs3 _ _ 0 = throwError EmptyArrayError
inputs3 sizeM sizeN sizeO = do
  vars <- replicateM sizeM (inputs2 sizeN sizeO)
  return $ toArray vars

--------------------------------------------------------------------------------

-- | Typeclass for retrieving the element of an array
class Mutable t where
  -- | Allocates a fresh variable for a value
  alloc :: Val t n -> Comp n (Ref t)

  typeOf :: Val t n -> ElemType

  constructElementRef :: ElemType -> Addr -> Ref t

instance Mutable ref => Mutable ('ArrM ref) where
  alloc xs@(Ref (ArrayRef elemType len _)) = do
    elements <- mapM (accessM xs) [0 .. len - 1]
    allocArray elemType elements

  typeOf (Ref (ArrayRef elemType len _)) = ArrElem elemType len

  constructElementRef (ArrElem l k) elemAddr = ArrayRef l k elemAddr
  constructElementRef _ _ = error "expecting element to be array"

instance Mutable 'Num where
  alloc val = do
    var <- freshVar
    modify' $ \st -> st {compNumAsgns = Assignment (NumVar var) val : compNumAsgns st}
    return $ NumVar var

  typeOf _ = NumElem

  constructElementRef NumElem elemAddr = NumVar elemAddr
  constructElementRef _ _ = error "expecting element to be of Num"

instance Mutable 'Bool where
  alloc val = do
    var <- freshVar
    modify' $ \st -> st {compBoolAsgns = Assignment (BoolVar var) val : compBoolAsgns st}
    return $ BoolVar var

  typeOf _ = BoolElem

  constructElementRef BoolElem elemAddr = BoolVar elemAddr
  constructElementRef _ _ = error "expecting element to be of Bool"

-- | Access an element from a 1-D array
accessM :: Mutable t => Val ('ArrM t) n -> Int -> Comp n (Val t n)
accessM (Ref (ArrayRef _ _ addr)) i = Ref <$> readHeap (addr, i)

-- | Access an element from a 2-D array
accessM2 :: Mutable t => Val ('ArrM ('ArrM t)) n -> (Int, Int) -> Comp n (Val t n)
accessM2 addr (i, j) = accessM addr i >>= flip accessM j

-- | Access an element from a 3-D array
accessM3 :: Mutable t => Val ('ArrM ('ArrM ('ArrM t))) n -> (Int, Int, Int) -> Comp n (Val t n)
accessM3 addr (i, j, k) = accessM addr i >>= flip accessM j >>= flip accessM k

access :: Val ('Arr t) n -> Int -> Val t n
access (ArrayVal xs) i =
  if i < length xs
    then xs ! i
    else error $ show $ IndexOutOfBoundsError2 (length xs) i
access (Ref ref) _ = case ref of {}

-- | Access an element from a 2-D array
access2 :: Val ('Arr ('Arr t)) n -> (Int, Int) -> Val t n
access2 addr (i, j) = access (access addr i) j

-- | Access an element from a 3-D array
access3 :: Val ('Arr ('Arr ('Arr t))) n -> (Int, Int, Int) -> Val t n
access3 addr (i, j, k) = access (access (access addr i) j) k

--------------------------------------------------------------------------------

-- | Internal helper function extracting the address of a reference
addrOfRef :: Ref t -> Addr
addrOfRef (BoolVar addr) = addr
addrOfRef (NumVar addr) = addr
addrOfRef (ArrayRef _ _ addr) = addr

-- | Internal helper function for allocating an array with values
allocArray :: Mutable t => ElemType -> [Val t n] -> Comp n (Ref ('ArrM ty))
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
  modifyHeap (IntMap.insert addr (elemType, bindings))
  return addr

-- | Internal helper function for updating an array entry on the heap
writeHeap :: Addr -> ElemType -> (Int, Var) -> Comp n ()
writeHeap addr elemType (index, var) = do
  let bindings = IntMap.singleton index var
  modifyHeap (IntMap.insertWith (<>) addr (elemType, bindings))

modifyHeap :: (Heap -> Heap) -> Comp n ()
modifyHeap f = do
  heap <- gets compHeap
  let heap' = f heap
  modify (\st -> st {compHeap = heap'})

-- | Internal helper function for accessing an element of an array on the heap
readHeap :: Mutable t => (Addr, Int) -> Comp n (Ref t)
readHeap (addr, i) = do
  heap <- gets compHeap
  case IntMap.lookup addr heap of
    Nothing -> error "readHeap: address not found"
    Just (elemType, array) -> case IntMap.lookup i array of
      Nothing -> throwError $ IndexOutOfBoundsError addr i array
      Just n -> return $ constructElementRef elemType n

-- | Internal helper function for accessing an array on the heap
readHeapArray :: Mutable t => Addr -> Comp n [Ref t]
readHeapArray addr = do
  heap <- gets compHeap
  case IntMap.lookup addr heap of
    Nothing -> error "readHeap: address not found"
    Just (elemType, array) -> return $ map (constructElementRef elemType) (IntMap.elems array)

--------------------------------------------------------------------------------

-- | An alternative to 'foldM'
reduce :: Foldable m => Val t n -> m a -> (Val t n -> a -> Comp n (Val t n)) -> Comp n (Val t n)
reduce a xs f = foldM f a xs

lengthOfM :: Val ('ArrM t) n -> Int
lengthOfM (Ref (ArrayRef _ len _)) = len

lengthOf :: Val ('Arr t) n -> Int
lengthOf (ArrayVal xs) = length xs
lengthOf (Ref ref) = case ref of {}

--------------------------------------------------------------------------------

-- | Assert that the given expression is true
assert :: Val 'Bool n -> Comp n ()
assert expr = modify' $ \st -> st {compAssertions = expr : compAssertions st}
