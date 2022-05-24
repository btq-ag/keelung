{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Keelung.Monad
  ( Comp,
    runComp,
    elaborate_,
    Elaborable(..),
    Computation (..),
    Elaborated (..),
    Assignment (..),

    -- * Variable
    allocVar,

    -- * Array
    allocArray,
    access,
    access2,
    access3,
    slice,
    update,

    -- * Input Variable & Array
    inputVar,
    inputArray,
    inputArray2,
    inputArray3,

    -- * Statements
    ifThenElse,
    reduce,

    -- * Assertion
    assert,
    assertArrayEqual,

    -- * Other typeclasses
    Referable (..),
    Comparable (..),
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
import Control.Arrow (left)
import Data.Serialize (Serialize, encode)
import GHC.Generics (Generic)
import Data.ByteString (ByteString)

--------------------------------------------------------------------------------

-- | An Assignment associates an expression with a reference
data Assignment ty n = Assignment (Ref ('V ty)) (Expr ty n)
  deriving (Eq, Generic)

instance Show n => Show (Assignment ty n) where
  show (Assignment var expr) = show var <> " := " <> show expr

instance Functor (Assignment ty) where
  fmap f (Assignment var expr) = Assignment var (fmap f expr)

instance Serialize n => Serialize (Assignment 'Num n) where 
instance Serialize n => Serialize (Assignment 'Bool n) where 

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
  deriving (Generic)

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
    elabExpr :: !(Maybe (Expr ty n)),
    -- | The state of computation after elaboration
    elabComp :: Computation n
  }
  deriving (Generic)

instance (Show n, GaloisField n, Bounded n, Integral n) => Show (Elaborated ty n) where
  show (Elaborated expr comp) =
    "{\n expression: "
      ++ show (fmap (fmap N) expr)
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

class Elaborable ty where
  -- | Elaborates a Keelung program
  elaborate :: Comp n (Expr ty n) -> Either String (Elaborated ty n)
  -- | Encode a Keelung program
  generate :: Serialize n => Comp n (Expr ty n) -> ByteString 

instance Elaborable 'Num where 
  elaborate prog = do
    (expr, comp') <- left show $ runComp (Computation 0 0 mempty mempty mempty mempty mempty) prog
    return $ Elaborated (Just expr) comp'
  generate prog = encode $ elaborate prog

instance Elaborable 'Bool where 
  elaborate prog = do
    (expr, comp') <- left show $ runComp (Computation 0 0 mempty mempty mempty mempty mempty) prog
    return $ Elaborated (Just expr) comp'
  generate prog = encode $ elaborate prog

instance Elaborable 'Unit where 
  elaborate prog = do
    (_, comp') <- left show $ runComp (Computation 0 0 mempty mempty mempty mempty mempty) prog
    return $ Elaborated Nothing comp'
  generate prog = encode $ elaborate prog

-- | An alternative to 'elaborate' that returns '()' instead of 'Expr'
elaborate_ :: Comp n () -> Either String (Elaborated 'Unit n)
elaborate_ prog = do
  ((), comp') <- left show $ runComp (Computation 0 0 mempty mempty mempty mempty mempty) prog
  return $ Elaborated Nothing comp'

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

--------------------------------------------------------------------------------

-- | "Slice" an array from a higher dimension array
slice :: Int -> Ref ('A ('A ty)) -> Comp n (Ref ('A ty))
slice i (Array addr) = Array <$> readHeap (addr, i)

-- | Access a variable from a 1-D array
access :: Ref ('A ('V ty)) -> Int -> Comp n (Ref ('V ty))
access (Array addr) i = Variable <$> readHeap (addr, i)

-- | Access a variable from a 2-D array
access2 :: Ref ('A ('A ('V ty))) -> (Int, Int) -> Comp n (Ref ('V ty))
access2 addr (i, j) = slice i addr >>= flip access j

-- | Access a variable from a 3-D array
access3 :: Ref ('A ('A ('A ('V ty)))) -> (Int, Int, Int) -> Comp n (Ref ('V ty))
access3 addr (i, j, k) = slice i addr >>= slice j >>= flip access k

-- | Update array 'addr' at position 'i' to expression 'expr'
update :: Referable ty => Ref ('A ('V ty)) -> Int -> Expr ty n -> Comp n ()
update (Array addr) i (Var (Variable n)) = writeHeap addr [(i, n)]
update (Array addr) i expr = do
  ref <- allocVar
  writeHeap addr [(i, ref)]
  -- associate 'ref' with the expression
  assign (Variable ref) expr

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
  return $ Array addr
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
ifThenElse p x y = IfThenElse p <$> x <*> y

-- | An alternative to 'foldM'
reduce :: Foldable t => Expr ty n -> t a -> (Expr ty n -> a -> Comp n (Expr ty n)) -> Comp n (Expr ty n)
reduce a xs f = foldM f a xs

--------------------------------------------------------------------------------

-- | Assert that the given expression is true
assert :: Expr 'Bool n -> Comp n ()
assert expr = modify' $ \st -> st {compAssertions = expr : compAssertions st}

-- | Assert that two expressions are equal
assertArrayEqual :: Comparable ty => Int -> Ref ('A ('V ty)) -> Ref ('A ('V ty)) -> Comp n ()
assertArrayEqual len xs ys = forM_ [0 .. len - 1] $ \i -> do
  a <- access xs i
  b <- access ys i
  assert (Var a `equal` Var b)

