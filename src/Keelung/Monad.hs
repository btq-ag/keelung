{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Keelung.Monad
  ( Comp,
    Computation (..),
    Elaborated (..),
    elaborate,

    -- * Inputs
    input,
    inputField,
    inputBool,
    inputUInt,
    inputList,
    inputList2,
    inputList3,
    inputVec,
    inputVec2,
    inputVec3,

    -- * Mutable Array
    Mutable (updateM),
    toArrayM,
    fromArrayM,
    freeze,
    freeze2,
    freeze3,
    thaw,
    thaw2,
    thaw3,
    accessM,
    accessM2,
    accessM3,

    -- * Statements
    Reusable (..),
    cond,
    assert,
    mapI,
    reduce,
    performDivMod,
    assertDivMod,
  )
where

import Control.Arrow (left)
import Control.Monad.Except
import Control.Monad.State.Strict hiding (get, put)
import Data.Data (Proxy (..))
import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Traversable (mapAccumL)
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import GHC.TypeNats (KnownNat, natVal)
import Keelung.Data.Struct
import Keelung.Error
import Keelung.Heap
import Keelung.Syntax
import Keelung.Syntax.Counters
import Keelung.Syntax.Encode (encode', runHeapM)
import Keelung.Syntax.Encode.Syntax qualified as Encoding

--------------------------------------------------------------------------------

-- | Data structure for elaboration bookkeeping
data Computation = Computation
  { -- Variable bookkeeping
    compCounters :: !Counters,
    -- Size of allocated heap addresses
    compAddrSize :: Int,
    -- Heap for arrays
    compHeap :: Heap,
    -- Bindings to expressions
    compExprBindings :: Struct (IntMap Field) (IntMap Boolean) (IntMap Encoding.UInt),
    -- Assertions are expressions that are expected to be true
    compAssertions :: [Boolean],
    -- DivMod relations: dividend = divisor * quotient + remainder
    compDivModRelsU :: IntMap (Encoding.UInt, Encoding.UInt, Encoding.UInt, Encoding.UInt)
  }
  deriving (Eq)

instance Show Computation where
  show (Computation _ addrSize _ eb assertions _divModRelsU) =
    "{\n"
      <> "  Address size: "
      <> show addrSize
      ++ "\n  Bindings to expressions: \n"
      ++ show eb
      ++ "\n  Assertions: \n"
      ++ show assertions
      ++ "\n\
         \}"

--------------------------------------------------------------------------------

-- | The result of elaborating a computation
data Elaborated t = Elaborated
  { -- | The resulting expression
    elabExpr :: !t,
    -- | The state of computation after elaboration
    elabComp :: Computation
  }
  -- = ElaboratedNum Field Computation
  deriving (Eq)

instance Show t => Show (Elaborated t) where
  show (Elaborated expr comp) =
    "{\n expression: "
      ++ show expr
      ++ "\n  compuation state: \n"
      ++ indent (indent (show comp))
      ++ "\n}"
    where
      indent :: String -> String
      indent = unlines . map ("  " <>) . lines

--------------------------------------------------------------------------------

-- | The type of a Keelung program
type Comp = StateT Computation (Except ElabError)

-- | Elaborates a Keelung program
elaborate :: Comp t -> Either Error (Elaborated t)
elaborate prog = do
  (expr, comp) <- left ElabError $ runComp (Computation mempty 0 mempty mempty mempty mempty) prog
  return $ Elaborated expr comp

-- | How to run the 'Comp' monad
runComp :: Computation -> Comp a -> Either ElabError (a, Computation)
runComp comp f = runExcept (runStateT f comp)

modifyCounter :: (Counters -> Counters) -> Comp ()
modifyCounter f = modify (\comp -> comp {compCounters = f (compCounters comp)})

--------------------------------------------------------------------------------
-- Variable & Input Variable
--------------------------------------------------------------------------------

-- | Allocate a fresh Field variable.
freshVarF :: Comp Var
freshVarF = do
  counters <- gets compCounters
  let index = getCount OfIntermediate OfField counters
  modifyCounter $ addCount OfIntermediate OfField 1
  return index

freshVarB :: Comp Var
freshVarB = do
  counters <- gets compCounters
  let index = getCount OfIntermediate OfBoolean counters
  modifyCounter $ addCount OfIntermediate OfBoolean 1
  return index

freshVarU :: Width -> Comp Var
freshVarU width = do
  counters <- gets compCounters
  let index = getCount OfIntermediate (OfUInt width) counters
  modifyCounter $ addCount OfIntermediate (OfUInt width) 1
  return index

-- | Allocate a fresh input variable.
freshInputVar :: VarType -> Int -> Comp Var
freshInputVar vt n = do
  counters <- gets compCounters
  let index = getCount OfInput vt counters
  modifyCounter $ addCount OfInput vt n
  return index

--------------------------------------------------------------------------------

-- | Typeclass for operations on base types
class Proper t where
  -- | Request a single fresh input
  input :: Comp t

  -- | Request a list of fresh inputs
  --   default implementation simply applies `replicateM` on `input`
  inputList :: Int -> Comp [t]
  inputList size = replicateM size input

  -- | Conditional clause
  cond :: Boolean -> t -> t -> t

instance Proper Field where
  input = inputField

  -- \| Specialized implementation for Field
  inputList size = do
    start <- freshInputVar OfField size
    return $ map VarFI [start .. start + size - 1]

  cond = IfF

instance Proper Boolean where
  input = inputBool

  -- \| Specialized implementation for Boolean
  inputList size = do
    start <- freshInputVar OfBoolean size
    return $ map VarBI [start .. start + size - 1]

  cond = IfB

instance KnownNat w => Proper (UInt w) where
  input = inputUInt

  -- \| Specialized implementation for UInt
  inputList size = do
    start <- freshInputVar (OfUInt width) size
    return $ map VarUI [start .. start + size - 1]
    where
      width = fromIntegral (natVal (Proxy :: Proxy w))

  cond = IfU

-- | Requests a fresh Field input variable
inputField :: Comp Field
inputField = VarFI <$> freshInputVar OfField 1

-- | Requests a fresh Boolean input variable
inputBool :: Comp Boolean
inputBool = VarBI <$> freshInputVar OfBoolean 1

-- | Requests a fresh UInt input variable of some bit width
inputUInt :: forall w. KnownNat w => Comp (UInt w)
inputUInt = VarUI <$> freshInputVar (OfUInt width) 1
  where
    width = fromIntegral (natVal (Proxy :: Proxy w))

-- | Converts a list of values to an 1D-array
toArrayM :: Mutable t => [t] -> Comp (ArrM t)
toArrayM xs = do
  if null xs
    then snd <$> allocArray EmptyArr xs
    else
      let kind = typeOf (head xs)
       in snd <$> allocArray kind xs

-- | Convert an array into a list of expressions
fromArrayM :: Mutable t => ArrM t -> Comp [t]
fromArrayM ((ArrayRef _ _ addr)) = readHeapArray addr

--------------------------------------------------------------------------------

-- | Requests a 2D-array of fresh input variables
inputList2 :: Proper t => Int -> Int -> Comp [[t]]
inputList2 sizeM sizeN = replicateM sizeM (inputList sizeN)

-- | Requests a 3D-array of fresh input variables
inputList3 :: Proper t => Int -> Int -> Int -> Comp [[[t]]]
inputList3 sizeM sizeN sizeO = replicateM sizeM (inputList2 sizeN sizeO)

--------------------------------------------------------------------------------

-- | Vector version of 'inputs'
inputVec :: Proper t => Int -> Comp (Vector t)
inputVec size = Vec.fromList <$> inputList size

-- | Vector version of 'inputs2'
inputVec2 :: Proper t => Int -> Int -> Comp (Vector (Vector t))
inputVec2 sizeM sizeN = Vec.fromList <$> replicateM sizeM (inputVec sizeN)

-- | Vector version of 'inputs3'
inputVec3 :: Proper t => Int -> Int -> Int -> Comp (Vector (Vector (Vector t)))
inputVec3 sizeM sizeN sizeO = Vec.fromList <$> replicateM sizeM (inputVec2 sizeN sizeO)

--------------------------------------------------------------------------------

-- | Convert a mutable array to an immutable array
freeze :: Mutable t => ArrM t -> Comp [t]
freeze = fromArrayM

freeze2 :: Mutable t => ArrM (ArrM t) -> Comp [[t]]
freeze2 xs = do
  xs' <- fromArrayM xs
  mapM freeze xs'

freeze3 :: Mutable t => ArrM (ArrM (ArrM t)) -> Comp [[[t]]]
freeze3 xs = do
  xs' <- fromArrayM xs
  mapM freeze2 xs'

-- | Convert an immutable array to a mutable array
thaw :: Mutable t => [t] -> Comp (ArrM t)
thaw = toArrayM . toList

thaw2 :: Mutable t => [[t]] -> Comp (ArrM (ArrM t))
thaw2 xs = mapM thaw (toList xs) >>= toArrayM

thaw3 :: Mutable t => [[[t]]] -> Comp (ArrM (ArrM (ArrM t)))
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

  typeOf ((ArrayRef elemType len _)) = ElemArr elemType len

  constructElement (ElemArr l k) elemAddr = ArrayRef l k elemAddr
  constructElement EmptyArr elemAddr = ArrayRef EmptyArr 0 elemAddr
  constructElement _ _ = error "expecting element to be array"

  updateM (ArrayRef elemType _ addr) i expr = do
    (var, _) <- alloc expr
    writeHeap addr elemType (i, var)

instance Mutable Field where
  alloc val = do
    var <- freshVarF
    assignF var val
    return (var, VarF var)

  typeOf _ = ElemF

  updateM (ArrayRef _ _ addr) i (VarF n) = writeHeap addr ElemF (i, n)
  updateM (ArrayRef elemType _ addr) i expr = do
    (var, _) <- alloc expr
    writeHeap addr elemType (i, var)

  constructElement ElemF elemAddr = VarF elemAddr
  constructElement _ _ = error "expecting element to be of Num"

instance Mutable Boolean where
  alloc val = do
    var <- freshVarB
    assignB var val
    return (var, VarB var)

  typeOf _ = ElemB

  updateM (ArrayRef _ _ addr) i (VarB n) = writeHeap addr ElemB (i, n)
  updateM (ArrayRef elemType _ addr) i expr = do
    (var, _) <- alloc expr
    writeHeap addr elemType (i, var)

  constructElement ElemB elemAddr = VarB elemAddr
  constructElement _ _ = error "expecting element to be of Bool"

instance KnownNat w => Mutable (UInt w) where
  alloc val = do
    let width = widthOf val
    var <- freshVarU width
    heap <- gets compHeap
    let encoded = runHeapM heap (encode' val)
    assignU width var encoded
    return (var, VarU var)

  typeOf :: KnownNat w => UInt w -> ElemType
  typeOf val = ElemU (widthOf val)

  updateM (ArrayRef _ _ addr) i val@(VarU n) = writeHeap addr (ElemU (widthOf val)) (i, n)
  updateM (ArrayRef elemType _ addr) i expr = do
    (var, _) <- alloc expr
    writeHeap addr elemType (i, var)

  constructElement (ElemU _) elemAddr = VarU elemAddr
  constructElement _ _ = error "expecting element to be of UInt"

-- -- | Access an element from a 1-D array
accessM :: Mutable t => ArrM t -> Int -> Comp t
accessM ((ArrayRef _ _ addr)) i = readHeap (addr, i)

-- | Access an element from a 2-D array
accessM2 :: Mutable t => ArrM (ArrM t) -> (Int, Int) -> Comp t
accessM2 addr (i, j) = accessM addr i >>= flip accessM j

-- | Access an element from a 3-D array
accessM3 :: Mutable t => ArrM (ArrM (ArrM t)) -> (Int, Int, Int) -> Comp t
accessM3 addr (i, j, k) = accessM addr i >>= flip accessM j >>= flip accessM k

--------------------------------------------------------------------------------

-- | Internal helper function for allocating an array with values
allocArray :: Mutable t => ElemType -> [t] -> Comp (Addr, ArrM u)
allocArray elemType vals = do
  -- allocate a new array for holding the variables of these elements
  addr <- gets compAddrSize
  modify (\st -> st {compAddrSize = succ addr})
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
    var <- freshVarB
    assignB var val
    return (VarB var)

instance Reusable Field where
  reuse val = do
    var <- freshVarF
    assignF var val
    return (VarF var)

instance (Reusable t, Mutable t) => Reusable (ArrM t) where
  reuse = return

instance (Reusable t, Traversable f) => Reusable (f t) where
  reuse = mapM reuse

assignF :: Var -> Field -> Comp ()
assignF var expr = modify' $ \st -> st {compExprBindings = updateF (IntMap.insert var expr) (compExprBindings st)}

assignB :: Var -> Boolean -> Comp ()
assignB var expr = modify' $ \st -> st {compExprBindings = updateB (IntMap.insert var expr) (compExprBindings st)}

assignU :: Width -> Var -> Encoding.UInt -> Comp ()
assignU width var expr = modify' $ \st -> st {compExprBindings = updateU width (IntMap.insert var expr) (compExprBindings st)}

--------------------------------------------------------------------------------
-- Asserting DivMod relations
--------------------------------------------------------------------------------

-- | TODO: Replace this with methods from the `Integral` class
performDivMod :: forall w. KnownNat w => UInt w -> UInt w -> Comp (UInt w, UInt w)
performDivMod dividend divisor = do
  remainder <- freshVarU width
  quotient <- freshVarU width
  assertDivMod dividend divisor (VarU quotient) (VarU remainder)
  return (VarU quotient, VarU remainder)
  where
    width = fromIntegral (natVal (Proxy :: Proxy w))

-- | Assert that dividend = divisor * quotient + remainder
assertDivMod :: forall w. KnownNat w => UInt w -> UInt w -> UInt w -> UInt w -> Comp ()
assertDivMod dividend divisor quotient remainder = do
  heap <- gets compHeap
  let encoded = runHeapM heap $ (,,,) <$> encode' dividend <*> encode' divisor <*> encode' quotient <*> encode' remainder
  modify (\st -> st {compDivModRelsU = IntMap.insert width encoded (compDivModRelsU st)})
  where
    width = fromIntegral (natVal (Proxy :: Proxy w))
