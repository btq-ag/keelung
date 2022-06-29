{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Keelung.Monad
  ( Comp,
    runComp,
    Computation (..),
    Elaborated (..),
    Assignment (..),

    -- * Experimental
    allocArray',
    expose,

    -- * Array
    allocArray,

    allocArray2,
    allocArray3,
    access,
    access2,
    access3,
    update,
    update2,
    update3,

    lengthOf,

    -- * Input Variable & Array
    inputVar,
    inputVarNum,
    inputVarBool,
    inputArray,
    inputArray2,
    inputArray3,

    -- * Statements
    ifThenElse,
    reduce,
    reducei,
    -- loop,s
    loopi,
    sum',
    product',

    -- * Assertion
    assert,
    assertArrayEqual,

    -- * Other typeclasses
    Referable (..),
    Comparable (..),
  )
where

import Control.Monad.Except
import Control.Monad.State.Strict hiding (get, put)
import Data.Field.Galois (GaloisField)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Serialize
import GHC.Generics (Generic)
import Keelung.Error
import Keelung.Field
import Keelung.Syntax
import Prelude hiding (product, sum)

--------------------------------------------------------------------------------

-- | An Assignment associates an expression with a reference
data Assignment ty n = Assignment (Ref ('V ty)) (Expr ty n)
  deriving (Eq, Generic)

instance Show n => Show (Assignment ty n) where
  show (Assignment var expr) = show var <> " := " <> show expr

instance Functor (Assignment ty) where
  fmap f (Assignment var expr) = Assignment var (fmap f expr)

instance Serialize n => Serialize (Assignment 'Num n)

instance Serialize n => Serialize (Assignment 'Bool n)

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
    compAssertions :: [Expr 'Bool n]
  }
  deriving (Generic, Eq)

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

instance Serialize n => Serialize (Computation n)

--------------------------------------------------------------------------------

-- | The result of elaborating a computation
data Elaborated ty n = Elaborated
  { -- | The resulting 'Expr'
    elabExpr :: !(Expr ty n),
    -- | The state of computation after elaboration
    elabComp :: Computation n
  }
  deriving (Generic, Eq)

instance (Show n, GaloisField n, Bounded n, Integral n) => Show (Elaborated ty n) where
  show (Elaborated expr comp) =
    "{\n expression: "
      ++ show (fmap N expr)
      ++ "\n  compuation state: \n"
      ++ show comp
      ++ "\n}"

instance Serialize n => Serialize (Elaborated 'Num n)

instance Serialize n => Serialize (Elaborated 'Bool n)

instance Serialize n => Serialize (Elaborated 'Unit n)

--------------------------------------------------------------------------------

-- | The type of a Keelung program
type Comp n = StateT (Computation n) (Except Error)

-- | How to run the 'Comp' monad
runComp :: Computation n -> Comp n a -> Either Error (a, Computation n)
runComp comp f = runExcept (runStateT f comp)

--------------------------------------------------------------------------------
-- Variable & Input Variable
--------------------------------------------------------------------------------

-- | Allocate a fresh variable.
allocVar :: Comp n Int
allocVar = do
  index <- gets compNextVar
  modify (\st -> st {compNextVar = succ index})
  return index

-- | Requests a fresh input variable
inputVar :: Comp n (Ref ('V ty))
inputVar = do
  var <- allocVar
  markVarAsInput var
  return $ Variable var

-- | Requests a fresh Num input variable
inputVarNum :: Comp n (Ref ('V 'Num))
inputVarNum = inputVar

-- | Requests a fresh Bool input variable
inputVarBool :: Comp n (Ref ('V 'Bool))
inputVarBool = inputVar

--------------------------------------------------------------------------------
-- Array & Input Array
--------------------------------------------------------------------------------

-- | Allocates a 1D-array of fresh variables
allocArray' :: Referable kind => [Expr kind n] -> Comp n (Ref ('A ('V kind)))
allocArray' xs = do 
  let size = length xs
  when (size == 0) $ throwError EmptyArrayError
  -- declare new variables
  vars <- newVars size
  -- allocate a new array and associate it's content with the new variables
  array <- allocateArrayWithVars vars

  forM_ (zip [0 .. ] xs) $ \(i, x) -> do 
    update array i x 

  return array 

-- | Convert an array into a list of expressions 
expose :: Ref ('A ('V kind)) -> Comp n [Expr kind n]
expose (Array len addr) = do 
  forM [0 .. len - 1] $ \i -> do 
    Var . Variable <$> readHeap (addr, i)




-- allocArray' 0 = throwError EmptyArrayError
-- allocArray' size = do
--   -- declare new variables
--   vars <- newVars size
--   -- allocate a new array and associate it's content with the new variables
--   allocateArrayWithVars vars

-- | Allocates a 1D-array of fresh variables
allocArray :: Int -> Comp n (Ref ('A ty))
allocArray 0 = throwError EmptyArrayError
allocArray size = do
  -- declare new variables
  vars <- newVars size
  -- allocate a new array and associate it's content with the new variables
  allocateArrayWithVars vars

-- | Allocates a 2D-array of fresh variables
allocArray2 :: Int -> Int -> Comp n (Ref ('A ('A ty)))
allocArray2 0 _ = throwError EmptyArrayError
allocArray2 _ 0 = throwError EmptyArrayError
allocArray2 sizeM sizeN = do
  -- allocate `sizeM` arrays each of size `sizeN`
  innerArrays <- replicateM sizeM (allocArray sizeN)
  -- collect references of these arrays
  vars <- forM innerArrays $ \array -> do
    case array of Array _ addr -> return addr
  -- and allocate a new array with these references
  allocateArrayWithVars $ IntSet.fromList vars

-- | Allocates a 3D-array of fresh variables
allocArray3 :: Int -> Int -> Int -> Comp n (Ref ('A ('A ty)))
allocArray3 0 _ _ = throwError EmptyArrayError
allocArray3 _ 0 _ = throwError EmptyArrayError
allocArray3 _ _ 0 = throwError EmptyArrayError
allocArray3 sizeM sizeN sizeO = do
  -- allocate `sizeM` arrays each of size `sizeN * sizeO`
  innerArrays <- replicateM sizeM (allocArray2 sizeN sizeO)
  -- collect references of these arrays
  vars <- forM innerArrays $ \array -> do
    case array of Array _ addr -> return addr
  -- and allocate a new array with these references
  allocateArrayWithVars $ IntSet.fromList vars

--------------------------------------------------------------------------------

-- | Requests a 1D-array of fresh input variables
inputArray :: Int -> Comp n (Ref ('A ty))
inputArray 0 = throwError EmptyArrayError
inputArray size = do
  -- draw new variables and mark them as inputs
  vars <- newVars size
  markVarsAsInput vars
  -- allocate a new array and associate it's content with the new variables
  allocateArrayWithVars vars

-- | Requests a 2D-array of fresh input variables
inputArray2 :: Int -> Int -> Comp n (Ref ('A ('A ty)))
inputArray2 0 _ = throwError EmptyArrayError
inputArray2 _ 0 = throwError EmptyArrayError
inputArray2 sizeM sizeN = do
  -- allocate `sizeM` input arrays each of size `sizeN`
  innerArrays <- replicateM sizeM (inputArray sizeN)
  -- collect references of these arrays
  vars <- forM innerArrays $ \array -> do
    case array of Array _ addr -> return addr
  -- and allocate a new array with these references
  allocateArrayWithVars $ IntSet.fromList vars

-- | Requests a 3D-array of fresh input variables
inputArray3 :: Int -> Int -> Int -> Comp n (Ref ('A ('A ('A ty))))
inputArray3 0 _ _ = throwError EmptyArrayError
inputArray3 _ 0 _ = throwError EmptyArrayError
inputArray3 _ _ 0 = throwError EmptyArrayError
inputArray3 sizeM sizeN sizeO = do
  -- allocate `sizeM` input arrays each of size `sizeN * sizeO`
  innerArrays <- replicateM sizeM (inputArray2 sizeN sizeO)
  -- collect references of these arrays
  vars <- forM innerArrays $ \array -> do
    case array of Array _ addr -> return addr
  -- and allocate a new array with these references
  allocateArrayWithVars $ IntSet.fromList vars

--------------------------------------------------------------------------------

-- | Typeclass for retrieving the element of an array
-- The reason why we need a typeclass for this is that
-- the element may be a variable ('V) or another array ('A)
class Accessible a where
  access :: Int -> Ref ('A a) -> Comp n (Ref a)

instance Accessible ('A ref) where
  access i (Array len addr) = Array len <$> readHeap (addr, i)

instance Accessible ('V kind) where
  access i (Array _len addr) = Variable <$> readHeap (addr, i)

-- | Access a variable from a 2-D array
access2 :: (Int, Int) -> Ref ('A ('A ('V ty))) -> Comp n (Ref ('V ty))
access2 (i, j) addr = access i addr >>= access j

-- | Access a variable from a 3-D array
access3 :: (Int, Int, Int) -> Ref ('A ('A ('A ('V ty)))) -> Comp n (Ref ('V ty))
access3 (i, j, k) addr = access i addr >>= access j >>= access k

--------------------------------------------------------------------------------

-- | Update array 'addr' at position 'i' to expression 'expr'
update :: Referable ty => Ref ('A ('V ty)) -> Int -> Expr ty n -> Comp n ()
update (Array _ addr) i (Var (Variable n)) = writeHeap addr [(i, n)]
update (Array _ addr) i expr = do
  ref <- allocVar
  writeHeap addr [(i, ref)]
  -- associate 'ref' with the expression
  assign (Variable ref) expr

-- | Update array 'addr' at position '(j, i)' to expression 'expr'
update2 :: Referable ty => Ref ('A ('A ('V ty))) -> (Int, Int) -> Expr ty n -> Comp n ()
update2 ref (j, i) expr = do 
  ref' <- access i ref 
  update ref' j expr

-- | Update array 'addr' at position '(k, j, i)' to expression 'expr'
update3 :: Referable ty => Ref ('A ('A ('A ('V ty)))) -> (Int, Int, Int) -> Expr ty n -> Comp n ()
update3 ref (k, j, i) expr = do 
  ref' <- access i ref >>= access j
  update ref' k expr

--------------------------------------------------------------------------------

-- | Internal helper function for generating multiple fresh variables.
newVars :: Int -> Comp n IntSet
newVars n = do
  index <- gets compNextVar
  modify (\st -> st {compNextVar = n + index})
  return $ IntSet.fromDistinctAscList [index .. index + n - 1]

-- | Internal helper function for allocating an array
-- and associate the address with a set of variables
allocateArrayWithVars :: IntSet -> Comp n (Ref ('A ty))
allocateArrayWithVars vars = do
  let size = IntSet.size vars
  addr <- freshAddr
  writeHeap addr $ zip [0 .. pred size] $ IntSet.toList vars
  return $ Array size addr
  where
    freshAddr :: Comp n Addr
    freshAddr = do
      addr <- gets compNextAddr
      modify (\st -> st {compNextAddr = succ addr})
      return addr

-- | Internal helper function for marking a variable as input.
markVarAsInput :: Var -> Comp n ()
markVarAsInput = markVarsAsInput . IntSet.singleton

-- | Internal helper function for marking multiple variables as input
markVarsAsInput :: IntSet -> Comp n ()
markVarsAsInput vars =
  modify (\st -> st {compInputVars = vars <> compInputVars st})

-- | Internal helper function for allocating an array on the heap
writeHeap :: Addr -> [(Int, Var)] -> Comp n ()
writeHeap addr array = do
  let bindings = IntMap.fromList array
  heap <- gets compHeap
  let heap' = IntMap.insertWith (<>) addr bindings heap
  modify (\st -> st {compHeap = heap'})

-- | Internal helper function for access an array on the heap
readHeap :: (Addr, Int) -> Comp n Int
readHeap (addr, i) = do
  heap <- gets compHeap
  case IntMap.lookup addr heap of
    Nothing ->
      throwError $ UnboundArrayError addr i heap
    Just array -> case IntMap.lookup i array of
      Nothing -> throwError $ UnboundArrayError addr i heap
      Just n -> return n

--------------------------------------------------------------------------------

-- | Typeclass for certain operations on references
class Referable ty where
  assign :: Ref ('V ty) -> Expr ty n -> Comp n ()

instance Referable 'Num where
  assign var e = modify' $ \st -> st {compNumAsgns = Assignment var e : compNumAsgns st}

instance Referable 'Bool where
  assign var e = modify' $ \st -> st {compBoolAsgns = Assignment var e : compBoolAsgns st}

-- | Typeclass for comparing expressions
class Comparable ty where
  equal :: Expr ty n -> Expr ty n -> Expr 'Bool n

instance Comparable 'Num where
  equal x y = x `Eq` y

instance Comparable 'Bool where
  equal x y = x `BEq` y

--------------------------------------------------------------------------------

-- | Helper function for constructing the if...then...else expression
ifThenElse :: Expr 'Bool n -> Comp n (Expr ty n) -> Comp n (Expr ty n) -> Comp n (Expr ty n)
ifThenElse p x y = If p <$> x <*> y

-- | An alternative to 'foldM'
reduce :: Foldable t => Expr ty n -> t a -> (Expr ty n -> a -> Comp n (Expr ty n)) -> Comp n (Expr ty n)
reduce a xs f = foldM f a xs

-- reduce ::
--   Ref ('A ('V kind)) ->
--   Int ->
--   a ->
--   (a -> Ref ('V kind) -> Comp n a) ->
--   Comp n a
-- reduce xs len e f = reducei xs len e (const f)

-- | For aggregating some result of an array
--   the supplied function will be given
--      1. the current index
--      1. the current accumulator
--      3. the current element
reducei ::
  Ref ('A ('V kind)) ->
  a ->
  (Int -> a -> Ref ('V kind) -> Comp n a) ->
  Comp n a
reducei xs e f =
  foldM
    ( \acc i -> do
        x <- access i xs
        f i acc x
    )
    e
    [0 .. pred (lengthOf xs)]

lengthOf :: Ref ('A kind) -> Int 
lengthOf (Array n _) = n

-- | For iterating through an array
-- loop :: GaloisField n => Ref ('A ('V kind)) -> Int -> (Ref ('V kind) -> Comp n ()) -> Comp n ()
-- loop xs len f = reduce xs len () $ \_acc x -> do
--   _ <- f x
--   return ()

-- | For iterating through an array
loopi :: GaloisField n => Ref ('A ('V kind)) -> (Int -> Ref ('V kind) -> Comp n ()) -> Comp n ()
loopi xs f = reducei xs () $ \i _acc x -> do
  _ <- f i x
  return ()

-- | For iterating through an array of array
-- TODO: merge this with 'loop'
-- loopArr :: GaloisField n => Ref ('A ('A ref)) -> Int -> (Ref ('A ref) -> Comp n (Expr kind n)) -> Comp n ()
-- loopArr xs len f = forM_ [0 .. pred len] $ \i -> do
--   x <- slice i xs
--   f x
sum' :: GaloisField n => Ref ('A ('V 'Num)) -> Comp n (Expr 'Num n)
sum' xs = reducei xs 0 $ \_ acc x -> do
  return $ acc + Var x

product' :: GaloisField n => Ref ('A ('V 'Num)) -> Comp n (Expr 'Num n)
product' xs = reducei xs 1 $ \_ acc x -> do
  return $ acc * Var x

--------------------------------------------------------------------------------

-- | Assert that the given expression is true
assert :: Expr 'Bool n -> Comp n ()
assert expr = modify' $ \st -> st {compAssertions = expr : compAssertions st}

-- | Assert that two expressions are equal
assertArrayEqual :: Comparable ty => Int -> Ref ('A ('V ty)) -> Ref ('A ('V ty)) -> Comp n ()
assertArrayEqual len xs ys = forM_ [0 .. len - 1] $ \i -> do
  a <- access i xs
  b <- access i ys
  assert (Var a `equal` Var b)

--------------------------------------------------------------------------------