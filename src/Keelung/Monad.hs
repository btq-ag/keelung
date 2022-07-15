{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Keelung.Monad
  ( Comp,
    runComp,
    Computation (..),
    Elaborated (..),
    Assignment (..),
    --

    -- * Experimental

    -- allocArray',
    -- expose,

    -- * Array

    allocArray,
    -- allocArray2,
    -- allocArray3,
    -- access,
    -- access2,
    -- access3,
    update,
    -- update2,
    -- update3,
    -- lengthOf,
    Referable (..),

    -- * Input Variable & Array
    input,
    inputNum,
    inputBool,
    inputs,
    inputs2,
    inputs3,




    cond,

    -- * Statements
    assert,
    -- ifThenElse,
    -- reduce,
    -- reducei,
    -- -- loop,s
    -- loopi,
    -- sum',
    -- product',

    -- -- * Assertion
    -- assert,
    -- assertArrayEqual,

    -- -- * Other typeclasses
    -- Referable (..),
    -- Comparable (..),
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
data Assignment ty n = Assignment (Ref ty) (Expr ty n)
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

--------------------------------------------------------------------------------

-- | Typeclass for operations on base types
class Proper t where
  -- | Request a fresh input
  input :: Comp n (Expr t n)

  -- | Update an entry of an array
  update :: Expr ('Arr t) n -> Int -> Expr t n -> Comp n ()

  -- | Conditional clause
  cond :: Expr 'Bool n -> Expr t n -> Expr t n -> Expr t n

instance Proper 'Num where
  input = inputNum

  update (Val val) _ _ = case val of {}
  update (Ref (Array _ addr)) i (Ref (NumVar n)) = writeHeap addr [(i, n)]
  update (Ref (Array _ addr)) i expr = do
    ref <- allocVar
    writeHeap addr [(i, ref)]
    -- associate 'ref' with the expression
    assign ref expr

  cond = IfNum

instance Proper 'Bool where
  input = inputBool

  update (Val val) _ _ = case val of {}
  update (Ref (Array _ addr)) i (Ref (BoolVar n)) = writeHeap addr [(i, n)]
  update (Ref (Array _ addr)) i expr = do
    ref <- allocVar
    writeHeap addr [(i, ref)]
    -- associate 'ref' with the expression
    assign ref expr

  cond = IfBool

-- | Requests a fresh Num input variable
inputNum :: Comp n (Expr 'Num n)
inputNum = do
  var <- allocVar
  markVarAsInput var
  return $ Ref $ NumVar var

-- | Requests a fresh Bool input variable
inputBool :: Comp n (Expr 'Bool n)
inputBool = do
  var <- allocVar
  markVarAsInput var
  return $ Ref $ BoolVar var

--------------------------------------------------------------------------------
-- Array & Input Array
--------------------------------------------------------------------------------

-- | Allocates a 1D-array of fresh variables
allocArray :: Referable t => [Expr t n] -> Comp n (Expr ('Arr t) n)
allocArray xs = do
  let size = length xs
  when (size == 0) $ throwError EmptyArrayError
  -- declare new variables
  vars <- newVars size
  -- alloc ate a new array and associate it's content with the new variables
  arrayAddr <- allocateArrayWithVars vars

  forM_ (zip (IntSet.toList vars) xs) $ \(var, expr) -> do
    -- associate each variable with the corresponding element of the array
    assign var expr

  return (Ref arrayAddr)

-- | Convert an array into a list of expressions
-- toList :: Expr ('Arr t) n -> Comp n [Expr t n]
-- toList (Val val) = case val of {}
-- toList (Ref (Array len addr)) = do

--   elems <- forM [0 .. pred len] $ \i -> do
--     readHeap (addr, i)

  -- return $ map (Ref . NumVar) elems

  -- -- read the array from the heap
  -- elems <- readHeap addr
  -- -- convert each element into an expression
  -- return $ map (\(i, n) -> Ref $ Array len n) elems

-- -- | Allocates a 3D-array of fresh variables
-- allocArray3 :: Int -> Int -> Int -> Comp n (Ref ('Arr ('Arr ('Arr kind))))
-- allocArray3 0 _ _ = throwError EmptyArrayError
-- allocArray3 _ 0 _ = throwError EmptyArrayError
-- allocArray3 _ _ 0 = throwError EmptyArrayError
-- allocArray3 sizeM sizeN sizeO = do
--   -- allocate `sizeM` arrays each of size `sizeN * sizeO`
--   innerArrays <- replicateM sizeM (allocArray2 sizeN sizeO)
--   -- collect references of these arrays
--   vars <- forM innerArrays $ \array -> do
--     case array of Array2 _ addr -> return addr
--   -- and allocate a new array with these references
--   allocateArrayWithVars $ IntSet.fromList vars

--------------------------------------------------------------------------------

-- | Requests a 1D-array of fresh input variables
inputs :: (Proper t, Referable t) => Int -> Comp n (Expr ('Arr t) n)
inputs 0 = throwError EmptyArrayError
inputs size = do
  vars <- replicateM size input
  allocArray vars

-- | Requests a 2D-array of fresh input variables
inputs2 :: (Proper t, Referable t) => Int -> Int -> Comp n (Expr ('Arr ('Arr t)) n)
inputs2 0 _ = throwError EmptyArrayError
inputs2 _ 0 = throwError EmptyArrayError
inputs2 sizeM sizeN = do
  vars <- replicateM sizeM (inputs sizeN)
  allocArray vars

-- | Requests a 3D-array of fresh input variables
inputs3 :: (Proper t, Referable t) => Int -> Int -> Int -> Comp n (Expr ('Arr ('Arr ('Arr t))) n)
inputs3 0 _ _ = throwError EmptyArrayError
inputs3 _ 0 _ = throwError EmptyArrayError
inputs3 _ _ 0 = throwError EmptyArrayError
inputs3 sizeM sizeN sizeO = do
  vars <- replicateM sizeM (inputs2 sizeN sizeO)
  allocArray vars

--------------------------------------------------------------------------------

-- | Typeclass for retrieving the element of an array
class Referable t where
  access :: Ref ('Arr t) -> Int -> Comp n (Expr t n)

  -- | Associates a variable with an expression
  assign :: Addr -> Expr t n -> Comp n ()

instance Referable ('Arr ref) where
  access (Array len addr) i = Ref . Array len <$> readHeap (addr, i)

  assign _ (Val val) = case val of {}
  assign ref (Ref (Array len addr)) = do
    forM_ [0 .. len - 1] $ \i -> do
      var <- readHeap (addr, i)
      writeHeap ref [(i, var)]

instance Referable 'Num where
  access (Array _len addr) i = Ref . NumVar <$> readHeap (addr, i)
  assign ref expr = modify' $ \st -> st {compNumAsgns = Assignment (NumVar ref) expr : compNumAsgns st}

instance Referable 'Bool where
  access (Array _len addr) i = Ref . BoolVar <$> readHeap (addr, i)
  assign ref expr = modify' $ \st -> st {compBoolAsgns = Assignment (BoolVar ref) expr : compBoolAsgns st}

-- -- | Access a variable from a 2-D array
-- access2 :: Ref ('Arr ('Arr kind)) -> (Int, Int) -> Comp n (Ref kind)
-- access2 addr (i, j) = access addr i >>= flip access j

-- -- | Access a variable from a 3-D array
-- access3 :: Ref ('Arr ('Arr ('Arr kind))) -> (Int, Int, Int) -> Comp n (Ref kind)
-- access3 addr (i, j, k) = access addr i >>= flip access j >>= flip access k

--------------------------------------------------------------------------------

-- -- | Update array 'addr' at position '(j, i)' to expression 'expr'
-- update2 :: Referable ty => Ref ('A ('A ('V ty))) -> (Int, Int) -> Expr ty n -> Comp n ()
-- update2 ref (j, i) expr = do
--   ref' <- access ref i
--   update ref' j expr

-- -- | Update array 'addr' at position '(k, j, i)' to expression 'expr'
-- update3 :: Referable ty => Ref ('A ('A ('A ('V ty)))) -> (Int, Int, Int) -> Expr ty n -> Comp n ()
-- update3 ref (k, j, i) expr = do
--   ref' <- access ref i >>= flip access j
--   update ref' k expr

-- --------------------------------------------------------------------------------

-- | Internal helper function for generating multiple fresh variables.
newVars :: Int -> Comp n IntSet
newVars n = do
  index <- gets compNextVar
  modify (\st -> st {compNextVar = n + index})
  return $ IntSet.fromDistinctAscList [index .. index + n - 1]

-- | Internal helper function for allocating an array
-- and associate the address with a set of variables
allocateArrayWithVars :: IntSet -> Comp n (Ref ('Arr ty))
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

-- --------------------------------------------------------------------------------

-- -- | Typeclass for certain operations on references
-- class Referable ty where
--   assign :: Ref ('V ty) -> Expr ty n -> Comp n ()

-- instance Referable 'Num where
--   assign var e = modify' $ \st -> st {compNumAsgns = Assignment var e : compNumAsgns st}

-- instance Referable 'Bool where
--   assign var e = modify' $ \st -> st {compBoolAsgns = Assignment var e : compBoolAsgns st}

-- -- | Typeclass for comparing expressions
-- class Comparable ty where
--   equal :: Expr ty n -> Expr ty n -> Expr 'Bool n

-- instance Comparable 'Num where
--   equal x y = x `Eq` y

-- instance Comparable 'Bool where
--   equal x y = x `BEq` y

-- --------------------------------------------------------------------------------

-- -- | Helper function for constructing the if...then...else expression
-- ifThenElse :: Expr 'Bool n -> Comp n (Expr ty n) -> Comp n (Expr ty n) -> Comp n (Expr ty n)
-- ifThenElse p x y = If p <$> x <*> y

-- -- | An alternative to 'foldM'
-- reduce :: Foldable t => Expr ty n -> t a -> (Expr ty n -> a -> Comp n (Expr ty n)) -> Comp n (Expr ty n)
-- reduce a xs f = foldM f a xs

-- -- reduce ::
-- --   Ref ('A ('V kind)) ->
-- --   Int ->
-- --   a ->
-- --   (a -> Ref ('V kind) -> Comp n a) ->
-- --   Comp n a
-- -- reduce xs len e f = reducei xs len e (const f)

-- -- | For aggregating some result of an array
-- --   the supplied function will be given
-- --      1. the current index
-- --      1. the current accumulator
-- --      3. the current element
-- reducei ::
--   Ref ('A ('V kind)) ->
--   a ->
--   (Int -> a -> Ref ('V kind) -> Comp n a) ->
--   Comp n a
-- reducei xs e f =
--   foldM
--     ( \acc i -> do
--         x <- access xs i
--         f i acc x
--     )
--     e
--     [0 .. pred (lengthOf xs)]

-- lengthOf :: Ref ('A kind) -> Int
-- lengthOf (Array n _) = n

-- -- | For iterating through an array
-- -- loop :: GaloisField n => Ref ('A ('V kind)) -> Int -> (Ref ('V kind) -> Comp n ()) -> Comp n ()
-- -- loop xs len f = reduce xs len () $ \_acc x -> do
-- --   _ <- f x
-- --   return ()

-- -- | For iterating through an array
-- loopi :: GaloisField n => Ref ('A ('V kind)) -> (Int -> Ref ('V kind) -> Comp n ()) -> Comp n ()
-- loopi xs f = reducei xs () $ \i _acc x -> do
--   _ <- f i x
--   return ()

-- -- | For iterating through an array of array
-- -- TODO: merge this with 'loop'
-- -- loopArr :: GaloisField n => Ref ('A ('A ref)) -> Int -> (Ref ('A ref) -> Comp n (Expr kind n)) -> Comp n ()
-- -- loopArr xs len f = forM_ [0 .. pred len] $ \i -> do
-- --   x <- slice i xs
-- --   f x
-- sum' :: GaloisField n => Ref ('A ('V 'Num)) -> Comp n (Expr 'Num n)
-- sum' xs = reducei xs 0 $ \_ acc x -> do
--   return $ acc + Var x

-- product' :: GaloisField n => Ref ('A ('V 'Num)) -> Comp n (Expr 'Num n)
-- product' xs = reducei xs 1 $ \_ acc x -> do
--   return $ acc * Var x

--------------------------------------------------------------------------------

-- | Assert that the given expression is true
assert :: Expr 'Bool n -> Comp n ()
assert expr = modify' $ \st -> st {compAssertions = expr : compAssertions st}

-- -- | Assert that two expressions are equal
-- assertArrayEqual :: Comparable ty => Int -> Ref ('A ('V ty)) -> Ref ('A ('V ty)) -> Comp n ()
-- assertArrayEqual len xs ys = forM_ [0 .. len - 1] $ \i -> do
--   a <- access xs i
--   b <- access ys i
--   assert (Var a `equal` Var b)

-- --------------------------------------------------------------------------------