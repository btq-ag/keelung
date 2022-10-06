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
    Mutable (updateM),
    toArrayM,
    toArray,
    toArray',
    fromArray,
    fromArrayM,
    freeze,
    freeze2,
    freeze3,
    thaw,
    thaw2,
    thaw3,
    lengthOf,
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
    mapI,
    reduce,
    reuse,
  )
where

import Control.Monad.Except
import Control.Monad.State.Strict hiding (get, put)
import Data.Array ((!))
import Data.Array.Unboxed (Array)
import qualified Data.Array.Unboxed as IArray
import Data.Foldable (toList)
import qualified Data.IntMap.Strict as IntMap
import Keelung.Error
import Keelung.Syntax
import Keelung.Types
import Prelude hiding (product, sum)
import Data.Traversable (mapAccumL)

--------------------------------------------------------------------------------

-- | An Assignment associates an expression with a reference
data Assignment
  = BoolAssignment Var Boolean
  | NumAssignment Var Number
  deriving (Eq)

instance Show Assignment where
  show (BoolAssignment var expr) = show var <> " := " <> show expr
  show (NumAssignment var expr) = show var <> " := " <> show expr

--------------------------------------------------------------------------------

-- | Data structure for elaboration bookkeeping
data Computation = Computation
  { -- Counter for generating fresh variables
    compNextVar :: Int,
    -- Counter for generating fresh input variables
    compNextInputVar :: Int,
    -- Counter for allocating fresh heap addresses
    compNextAddr :: Int,
    -- Heap for arrays
    compHeap :: Heap,
    -- Assignments
    compNumAsgns :: [Assignment],
    compBoolAsgns :: [Assignment],
    -- Assertions are expressions that are expected to be true
    compAssertions :: [Boolean]
  }
  deriving (Eq)

emptyComputation :: Computation
emptyComputation = Computation 0 0 0 mempty mempty mempty mempty

instance Show Computation where
  show (Computation nextVar nextInputVar nextAddr _ numAsgns boolAsgns assertions) =
    "{\n  variable counter: " ++ show nextVar
      ++ "\n  input variable counter: "
      ++ show nextInputVar
      ++ "\n  address counter: "
      ++ show nextAddr
      ++ "\n  input variables: "
      ++ showInputVars
      ++ "\n  num assignments: "
      ++ show numAsgns
      ++ "\n  bool assignments: "
      ++ show boolAsgns
      ++ "\n  assertions: "
      ++ show assertions
      ++ "\n\
         \}"
    where
      showInputVars = case nextInputVar of
        0 -> "none"
        1 -> "$0"
        _ -> "[ $0 .. $" ++ show (nextInputVar - 1) ++ " ]"

--------------------------------------------------------------------------------

-- | The result of elaborating a computation
data Elaborated t = Elaborated
  { -- | The resulting expression
    elabExpr :: !t,
    -- | The state of computation after elaboration
    elabComp :: Computation
  }
  -- = ElaboratedNum Number Computation
  deriving (Eq)

instance Show t => Show (Elaborated t) where
  show (Elaborated expr comp) =
    "{\n expression: "
      ++ show expr
      ++ "\n  compuation state: \n"
      ++ show comp
      ++ "\n}"

--------------------------------------------------------------------------------

-- | The type of a Keelung program
type Comp = StateT Computation (Except ElabError)

-- | How to run the 'Comp' monad
runComp :: Computation -> Comp a -> Either ElabError (a, Computation)
runComp comp f = runExcept (runStateT f comp)

--------------------------------------------------------------------------------
-- Variable & Input Variable
--------------------------------------------------------------------------------

-- | Allocate a fresh variable.
freshVar :: Comp Var
freshVar = do
  index <- gets compNextVar
  modify (\st -> st {compNextVar = succ index})
  return index

-- | Allocate a fresh input variable.
freshInputVar :: Comp Var
freshInputVar = do
  index <- gets compNextInputVar
  modify (\st -> st {compNextInputVar = succ index})
  return index

--------------------------------------------------------------------------------

-- | Typeclass for operations on base types
class Proper t where
  -- | Request a fresh input
  input :: Comp t

  -- | Conditional clause
  cond :: Boolean -> t -> t -> t

instance Proper Number where
  input = inputNum
  cond = IfNum

instance Proper Boolean where
  input = inputBool
  cond = IfBool

-- | Requests a fresh Num input variable
inputNum :: Comp Number
inputNum = NumInputVar <$> freshInputVar

-- | Requests a fresh Bool input variable
inputBool :: Comp Boolean
inputBool = BoolInputVar <$> freshInputVar

--------------------------------------------------------------------------------
-- Array & Input Array
--------------------------------------------------------------------------------

-- | Converts a list of values to an 1D-array
toArrayM :: Mutable t => [t] -> Comp (ArrM t)
toArrayM xs = do
  when (null xs) $ throwError EmptyArrayError
  let kind = typeOf (head xs)
  snd <$> allocArray kind xs

-- | Immutable version of `toArray`
toArray :: [t] -> Arr t
toArray xs = Arr $ IArray.listArray (0, length xs - 1) xs

-- | Immutable version of `toArray`
toArray' :: Array Int t -> Arr t
toArray' = Arr

-- | Convert an array into a list of expressions
fromArrayM :: Mutable t => ArrM t -> Comp [t]
fromArrayM ((ArrayRef _ _ addr)) = readHeapArray addr

fromArray :: Arr t -> [t]
fromArray (Arr xs) = toList xs

--------------------------------------------------------------------------------

-- | Requests a 1D-array of fresh input variables
inputs :: Proper t => Int -> Comp (Arr t)
inputs 0 = throwError EmptyArrayError
inputs size = do
  vars <- replicateM size input
  return $ toArray vars

-- | Requests a 2D-array of fresh input variables
inputs2 :: Proper t => Int -> Int -> Comp (Arr (Arr t))
inputs2 0 _ = throwError EmptyArrayError
inputs2 _ 0 = throwError EmptyArrayError
inputs2 sizeM sizeN = do
  vars <- replicateM sizeM (inputs sizeN)
  return $ toArray vars

-- | Requests a 3D-array of fresh input variables
inputs3 :: Proper t => Int -> Int -> Int -> Comp (Arr (Arr (Arr t)))
inputs3 0 _ _ = throwError EmptyArrayError
inputs3 _ 0 _ = throwError EmptyArrayError
inputs3 _ _ 0 = throwError EmptyArrayError
inputs3 sizeM sizeN sizeO = do
  vars <- replicateM sizeM (inputs2 sizeN sizeO)
  return $ toArray vars

--------------------------------------------------------------------------------

-- | Convert a mutable array to an immutable array
freeze :: Mutable t => ArrM t -> Comp (Arr t)
freeze xs = toArray <$> fromArrayM xs

freeze2 :: Mutable t => ArrM (ArrM t) -> Comp (Arr (Arr t))
freeze2 xs = do
  xs' <- fromArrayM xs
  toArray <$> mapM freeze xs'

freeze3 :: Mutable t => ArrM (ArrM (ArrM t)) -> Comp (Arr (Arr (Arr t)))
freeze3 xs = do
  xs' <- fromArrayM xs
  toArray <$> mapM freeze2 xs'

-- | Convert an immutable array to a mutable array
thaw :: Mutable t => Arr t -> Comp (ArrM t)
thaw = toArrayM . toList

thaw2 :: Mutable t => Arr (Arr t) -> Comp (ArrM (ArrM t))
thaw2 xs = mapM thaw (toList xs) >>= toArrayM

thaw3 :: Mutable t => Arr (Arr (Arr t)) -> Comp (ArrM (ArrM (ArrM t)))
thaw3 xs = mapM thaw2 (toList xs) >>= toArrayM

--------------------------------------------------------------------------------

-- | Typeclass for retrieving the element of an array
class Mutable t where
  -- | Allocates a fresh variable for a value
  alloc :: t -> Comp (Var, t)

  typeOf :: t -> ElemType

  -- | Update an entry of an array.
  updateM :: ArrM t -> Int -> t -> Comp ()

  constructElement :: ElemType -> Addr -> t

instance Mutable ref => Mutable (ArrM ref) where
  alloc xs@((ArrayRef elemType len _)) = do
    elements <- mapM (accessM xs) [0 .. len - 1]
    allocArray elemType elements

  typeOf ((ArrayRef elemType len _)) = ArrElem elemType len

  constructElement (ArrElem l k) elemAddr = ArrayRef l k elemAddr
  constructElement _ _ = error "expecting element to be array"

  updateM (ArrayRef elemType _ addr) i expr = do
    (var, _) <- alloc expr
    writeHeap addr elemType (i, var)

instance Mutable Number where
  alloc val = do
    var <- freshVar
    modify' $ \st -> st {compNumAsgns = NumAssignment var val : compNumAsgns st}
    return (var, NumVar var)

  typeOf _ = NumElem

  updateM (ArrayRef _ _ addr) i (NumVar n) = writeHeap addr NumElem (i, n)
  updateM (ArrayRef elemType _ addr) i expr = do
    (var, _) <- alloc expr
    writeHeap addr elemType (i, var)

  constructElement NumElem elemAddr = NumVar elemAddr
  constructElement _ _ = error "expecting element to be of Num"

instance Mutable Boolean where
  alloc val = do
    var <- freshVar
    modify' $ \st -> st {compBoolAsgns = BoolAssignment var val : compBoolAsgns st}
    return (var, BoolVar var)

  typeOf _ = BoolElem

  updateM (ArrayRef _ _ addr) i (BoolVar n) = writeHeap addr BoolElem (i, n)
  updateM (ArrayRef elemType _ addr) i expr = do
    (var, _) <- alloc expr
    writeHeap addr elemType (i, var)

  constructElement BoolElem elemAddr = BoolVar elemAddr
  constructElement _ _ = error "expecting element to be of Bool"

-- -- | Access an element from a 1-D array
accessM :: Mutable t => ArrM t -> Int -> Comp t
accessM ((ArrayRef _ _ addr)) i = readHeap (addr, i)

-- | Access an element from a 2-D array
accessM2 :: Mutable t => ArrM (ArrM t) -> (Int, Int) -> Comp t
accessM2 addr (i, j) = accessM addr i >>= flip accessM j

-- | Access an element from a 3-D array
accessM3 :: Mutable t => ArrM (ArrM (ArrM t)) -> (Int, Int, Int) -> Comp t
accessM3 addr (i, j, k) = accessM addr i >>= flip accessM j >>= flip accessM k

access :: Arr t -> Int -> t
access (Arr xs) i =
  if i < length xs
    then xs ! i
    else error $ show $ IndexOutOfBoundsError2 (length xs) i

-- | Access an element from a 2-D array
access2 :: Arr (Arr t) -> (Int, Int) -> t
access2 addr (i, j) = access (access addr i) j

-- | Access an element from a 3-D array
access3 :: Arr (Arr (Arr t)) -> (Int, Int, Int) -> t
access3 addr (i, j, k) = access (access (access addr i) j) k

--------------------------------------------------------------------------------

-- | Internal helper function for allocating an array with values
allocArray :: Mutable t => ElemType -> [t] -> Comp (Addr, ArrM u)
allocArray elemType vals = do
  -- allocate a new array for holding the variables of these elements
  addr <- gets compNextAddr
  modify (\st -> st {compNextAddr = succ addr})
  -- allocate new variables for each element
  addresses <- map fst <$> mapM alloc vals
  let bindings = IntMap.fromDistinctAscList $ zip [0 ..] addresses
  modifyHeap (IntMap.insert addr (elemType, bindings))
  return (addr, ArrayRef elemType (length vals) addr)

-- | Internal helper function for updating an array entry on the heap
writeHeap :: Addr -> ElemType -> (Int, Var) -> Comp ()
writeHeap addr elemType (index, ref) = do
  let bindings = IntMap.singleton index ref
  modifyHeap (IntMap.insertWith (<>) addr (elemType, bindings))

modifyHeap :: (Heap -> Heap) -> Comp ()
modifyHeap f = do
  heap <- gets compHeap
  let heap' = f heap
  modify (\st -> st {compHeap = heap'})

-- | Internal helper function for accessing an element of an array on the heap
readHeap :: Mutable t => (Addr, Int) -> Comp t
readHeap (addr, i) = do
  heap <- gets compHeap
  case IntMap.lookup addr heap of
    Nothing -> error "readHeap: address not found"
    Just (elemType, array) -> case IntMap.lookup i array of
      Nothing -> throwError $ IndexOutOfBoundsError addr i array
      Just var -> return $ constructElement elemType var

-- | Internal helper function for accessing an array on the heap
readHeapArray :: Mutable t => Addr -> Comp [t]
readHeapArray addr = do
  heap <- gets compHeap
  case IntMap.lookup addr heap of
    Nothing -> error "readHeap: address not found"
    Just (elemType, array) -> return $ map (constructElement elemType) (IntMap.elems array)

--------------------------------------------------------------------------------

-- | An alternative to 'foldM'
reduce :: Foldable m => t -> m a -> (t -> a -> Comp t) -> Comp t
reduce a xs f = foldM f a xs

-- | Map with index, basically 'mapi' in OCaml.
mapI :: Traversable f => (Int -> a -> b) -> f a -> f b
mapI f = snd . mapAccumL (\i x -> (i + 1, f i x)) 0

-- | Length of a mutable array
lengthOf :: ArrM t -> Int
lengthOf ((ArrayRef _ len _)) = len

--------------------------------------------------------------------------------

-- | Assert that the given expression is true
assert :: Boolean -> Comp ()
assert expr = modify' $ \st -> st {compAssertions = expr : compAssertions st}

--------------------------------------------------------------------------------

-- | Allow an expression to be referenced and reused in the future
class Reusable t where
  reuse :: t -> Comp t

instance Reusable Boolean where
  reuse val = do
    xs <- toArrayM [val]
    accessM xs 0

instance Reusable Number where
  reuse val = do
    xs <- toArrayM [val]
    accessM xs 0

instance Mutable t => Reusable (ArrM t) where
  reuse val = do
    xs <- toArrayM [val]
    accessM xs 0

instance Mutable t => Reusable (Arr t) where
  reuse arr = thaw arr >>= reuse >>= freeze