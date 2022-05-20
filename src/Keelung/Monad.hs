{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Keelung.Monad
  ( Comp,
    elaborate,
    elaborate_,
    Computation (..),
    Elaborated (..),
    Assignment (..),
    -- * Variable / Input Variable 
    allocVar,
    inputVar,
    -- * Array / Input Array  
    allocArray,
    inputArray,
    inputArray2,
    inputArray3,
  )
where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Field.Galois (GaloisField)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Keelung.Error
import Keelung.Field
import Keelung.Syntax

--------------------------------------------------------------------------------

-- | An Assignment associates an expression with a reference
data Assignment ty n = Assignment (Ref ('V ty)) (Expr ty n)

instance Show n => Show (Assignment ty n) where
  show (Assignment var expr) = show var <> " := " <> show expr

instance Functor (Assignment ty) where
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
    compAssertions :: [Expr 'Bool n]
  }

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
data Elaborated ty n = Elaborated
  { -- | The resulting 'Expr'
    elabExpr :: !(Expr ty n),
    -- | The state of computation after elaboration
    elabComp :: Computation n
  }

instance (Show n, GaloisField n, Bounded n, Integral n) => Show (Elaborated ty n) where
  show (Elaborated expr comp) =
    "{\n expression: "
      ++ show (fmap N expr)
      ++ "\n  compuation state: \n"
      ++ show comp
      ++ "\n}"

--------------------------------------------------------------------------------

-- | The type of a Keelung program
type Comp n = StateT (Computation n) (Except Error)

-- | How to run the 'Comp' monad
runComp :: Computation n -> Comp n a -> Either Error (a, Computation n)
runComp comp f = runExcept (runStateT f comp)

-- | Elaborates a Keelung program
elaborate :: Comp n (Expr ty n) -> Either Error (Elaborated ty n)
elaborate prog = do
  (expr, comp') <- runComp (Computation 0 0 mempty mempty mempty mempty mempty) prog
  return $ Elaborated expr comp'

-- | An alternative to 'elaborate' that returns '()' instead of 'Expr'
elaborate_ :: Comp n () -> Either Error (Elaborated 'Unit n)
elaborate_ prog = do
  ((), comp') <- runComp (Computation 0 0 mempty mempty mempty mempty mempty) prog
  return $ Elaborated unit comp'


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

--------------------------------------------------------------------------------
-- Array & Input Array
--------------------------------------------------------------------------------

-- | Allocate a fresh array
allocArray :: Int -> Comp n (Ref ('A ty))
allocArray 0 = throwError EmptyArrayError
allocArray size = do
  -- declare new variables
  vars <- newVars size
  -- allocate a new array and associate it's content with the new variables
  allocateArrayWithVars vars

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
inputArray2 sizeM sizeN = do
  -- allocate `sizeM` input arrays each of size `sizeN`
  innerArrays <- replicateM sizeM (inputArray sizeN)
  -- collect references of these arrays
  vars <- forM innerArrays $ \array -> do
    case array of Array addr -> return addr
  -- and allocate a new array with these references
  allocateArrayWithVars $ IntSet.fromList vars

-- | Requests a 3D-array of fresh input variables
inputArray3 :: Int -> Int -> Int -> Comp n (Ref ('A ('A ('A ty))))
inputArray3 0 _ _ = throwError EmptyArrayError
inputArray3 sizeM sizeN sizeO = do
  -- allocate `sizeM` input arrays each of size `sizeN * sizeO`
  innerArrays <- replicateM sizeM (inputArray2 sizeN sizeO)
  -- collect references of these arrays
  vars <- forM innerArrays $ \array -> do
    case array of Array addr -> return addr
  -- and allocate a new array with these references
  allocateArrayWithVars $ IntSet.fromList vars



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
  return $ Array addr

-- | Internal helper function for allocating a fresh address
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
-- Inputs variables 
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Variables & Arrays 
--------------------------------------------------------------------------------



-- --------------------------------------------------------------------------------

-- -- | Typeclass for certain operations on references
-- class Referable ty where
--   assign :: Ref ('V ty) -> Expr ty n -> Comp n ()
--   arrayEq :: Int -> Ref ('A ('V ty)) -> Ref ('A ('V ty)) -> Comp n ()

-- instance Referable 'Num where
--   assign var e = modify' $ \st -> st {compNumAsgns = Assignment var e : compNumAsgns st}
--   arrayEq len xs ys = forM_ [0 .. len - 1] $ \i -> do
--     a <- access xs i
--     b <- access ys i
--     assert (Var a `Eq` Var b)

-- instance Referable 'Bool where
--   assign var e = modify' $ \st -> st {compBoolAsgns = Assignment var e : compBoolAsgns st}
--   arrayEq len xs ys = forM_ [0 .. len - 1] $ \i -> do
--     a <- access xs i
--     b <- access ys i
--     assert (Var a `BEq` Var b)
