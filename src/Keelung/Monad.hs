{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
{-# LANGUAGE LambdaCase #-}

-- | Monad and statements for building Keelung programs
module Keelung.Monad
  ( -- * Monad
    Comp,

    -- * Statements
    assert,
    performDivMod,
    assertDivMod,
    performCLDivMod,
    assertCLDivMod,
    assertLTE,
    assertLT,
    assertGTE,
    assertGT,
    toUInt,
    toField,
    pack,
    fromBools,
    fromField,
    SideEffect (..),

    -- * Inputs
    Input (..),
    -- Inputable (..),
    -- Pub,
    -- Prv,
    -- getVar,
    Encodeable(..),
    Proper (..),
    freshVarField,
    freshVarBool,
    freshVarUInt,
    InputAccess (..),
    inputField,
    inputBool,
    inputUInt,
    inputList,
    inputList2,
    inputList3,
    inputVec,
    inputVec2,
    inputVec3,

    -- * Reuse of expressions
    Reusable (..),

    -- * Combinators
    mapI,
    reduce,

    -- * Mutable Array
    ArrM,
    Mutable,
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
    updateM,
    lengthOf,

    -- * Types
    Computation (..),
    Elaborated (..),
    elaborate,
  )
where

import Control.Arrow (left)
import Control.Monad.Except
import Control.Monad.State.Strict hiding (get, put)
import Data.Data (Proxy (..))
import Data.IntMap.Strict qualified as IntMap
import qualified Data.Vector as Vec
import Data.Vector (Vector)
import Data.Sequence (Seq ((:|>)))
import Data.Sequence qualified as Seq
import Data.Traversable (mapAccumL)
import GHC.Generics hiding (UInt)
import GHC.TypeLits (KnownNat, natVal)
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
    -- Assertions are expressions that are expected to be true
    compAssertions :: [Boolean],
    -- Store side effects of the computation in a sequence so that we can simulate them during interpretation
    compSideEffects :: Seq SideEffect
  }
  deriving (Eq)

instance Show Computation where
  show (Computation _ addrSize _ assertions _) =
    "{\n"
      <> "  Address size: "
      <> show addrSize
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

instance (Show t) => Show (Elaborated t) where
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
  (expr, comp) <- left ElabError $ runComp (Computation mempty 0 mempty mempty mempty) prog
  return $ Elaborated expr comp

-- | How to run the 'Comp' monad
runComp :: Computation -> Comp a -> Either ElabError (a, Computation)
runComp comp f = runExcept (runStateT f comp)

modifyCounter :: (Counters -> Counters) -> Comp ()
modifyCounter f = modify (\comp -> comp {compCounters = f (compCounters comp)})

--------------------------------------------------------------------------------
-- Variable & Input Variable
--------------------------------------------------------------------------------

-- | Modifier for input variables
--
--   @since 0.8.4.0
data InputAccess
  = -- | For public input variables, visible to the prover and the verifier
    Public
  | -- | For private input variables, visible to the prover only
    Private

-- | Allocate a fresh 'Field' variable.
--
--   @since 0.8.4.0
freshVarF :: Comp Var
freshVarF = do
  counters <- gets compCounters
  let index = getCount counters (Intermediate, ReadField)
  modifyCounter $ addCount (Intermediate, WriteField) 1
  return index

-- | Allocate a fresh 'Boolean' variable.
--
--   @since 0.8.4.0
freshVarB :: Comp Var
freshVarB = do
  counters <- gets compCounters
  let index = getCount counters (Intermediate, ReadBool)
  modifyCounter $ addCount (Intermediate, WriteBool) 1
  return index

-- | Allocate a fresh 'UInt' variable.
--
--   @since 0.8.4.0
freshVarU :: Width -> Comp Var
freshVarU width = do
  counters <- gets compCounters
  let index = getCount counters (Intermediate, ReadUInt width)
  modifyCounter $ addCount (Intermediate, WriteUInt width) 1
  return index

-- | Allocate a fresh input variable.
freshInputVar :: InputAccess -> ReadType -> WriteType -> Int -> Comp Var
freshInputVar acc readType writeType n = do
  counters <- gets compCounters
  case acc of
    Public -> do
      let index = getCount counters (PublicInput, readType)
      modifyCounter $ addCount (PublicInput, writeType) n
      return index
    Private -> do
      let index = getCount counters (PrivateInput, readType)
      modifyCounter $ addCount (PrivateInput, writeType) n
      return index

--------------------------------------------------------------------------------

-- Problem: Generic instances for base types are wrong.
-- Solution 1: redefine Generic instances
-- Solution 2: check if a representation is a base type

-- just gives pure inner value, while ginput performs input at the same time
class GEncodeable f where
  ginput :: InputAccess -> Comp (f x)
  gtoInts :: f x -> [ Integer ]

instance {-# OVERLAPS #-} GEncodeable (Rec0 Field) where
  ginput acc = K1 <$> inputField acc
  gtoInts (K1 f) = toInts f

instance {-# OVERLAPS #-} GEncodeable (Rec0 Boolean) where
  ginput acc = K1 <$> inputBool acc
  gtoInts (K1 b) = toInts b

instance {-# OVERLAPS #-} KnownNat w => GEncodeable (Rec0 (UInt w)) where
  ginput acc = K1 <$> inputUInt acc
  gtoInts (K1 i) = toInts i

-- instance of (a :+: b) is deliberatly missing so the size of type is deterministic.

-- flatten all elements into 1-d array
instance GEncodeable U1 where
  ginput _ = return U1
  gtoInts = const []

instance (GEncodeable a, GEncodeable b) => GEncodeable (a :*: b) where
  ginput acc = do
    a' <- ginput acc
    b' <- ginput acc
    return (a' :*: b')
  gtoInts (x :*: y) = gtoInts x ++ gtoInts y

instance (GEncodeable a) => GEncodeable (M1 i c a) where
  ginput acc = do
    a <- ginput acc 
    return (M1 a)
  gtoInts (M1 a) = gtoInts a

instance (Encodeable a) => GEncodeable (K1 i a) where
  ginput acc = K1 <$> inputData acc
  gtoInts (K1 k) = toInts k

class Encodeable a where
  inputData :: InputAccess -> Comp a
  toInts :: a -> [ Integer ]
  default inputData :: (Generic a, GEncodeable (Rep a)) => InputAccess -> Comp a
  inputData acc = to <$> ginput acc
  default toInts :: (Generic a, GEncodeable (Rep a)) => a -> [ Integer ]
  toInts = gtoInts . from
 
instance Encodeable Field where
  inputData = inputField
  toInts = \case
      (Integer i) -> [ i ]
      _ -> error "toInts should not be used here."

instance Encodeable Boolean where
  inputData = inputBool
  toInts = \case
      (Boolean b) -> [ if b then 1 else 0 ]
      _ -> error "toInts should not be used here."

instance (KnownNat w) => Encodeable (UInt w) where
  inputData = inputUInt 
  toInts = \case
      (UInt j) -> [ j ]
      _ -> error "toInts should not be used here."
 
instance Encodeable ()
instance (Encodeable a) => Encodeable (Proxy a)
instance (Encodeable a, Encodeable b) => Encodeable (a, b)
instance (Encodeable a, Encodeable b, Encodeable c) => Encodeable (a, b, c)
instance (Encodeable a, Encodeable b, Encodeable c, Encodeable d) => Encodeable (a, b, c, d)
instance (Encodeable a, Encodeable b, Encodeable c, Encodeable d, Encodeable e) => Encodeable (a, b, c, d, e)
instance (Encodeable a, Encodeable b, Encodeable c, Encodeable d, Encodeable e, Encodeable f) => Encodeable (a, b, c, d, e, f)
instance (Encodeable a, Encodeable b, Encodeable c, Encodeable d, Encodeable e, Encodeable f, Encodeable g) => Encodeable (a, b, c, d, e, f, g)

class Input t where
  -- | Request a fresh input variable
  --
  --   @since 0.1.0.0
  input :: InputAccess -> Comp t

class Proper t where
  -- | Request a fresh variable
  --
  --   @since 0.8.4.0
  freshVar :: Comp t

  -- | Conditional clause
  --
  --   @since 0.1.0.0
  cond :: Boolean -> t -> t -> t

instance Input Field where
  input = inputField

instance Proper Field where

  cond = IfF
  freshVar = VarF <$> freshVarF

instance Input Boolean where
  input = inputBool

instance Proper Boolean where

  cond = IfB
  freshVar = VarB <$> freshVarB

instance (KnownNat w) => Input (UInt w) where
  input = inputUInt

instance (KnownNat w) => Proper (UInt w) where

  cond = IfU
  freshVar = VarU <$> freshVarU width
    where
      width = fromIntegral (natVal (Proxy :: Proxy w))

inputList :: (Input t) => InputAccess -> Int -> Comp [t]
inputList acc len = replicateM len (input acc)

-- | Requests a fresh 'Field' input variable
inputField :: InputAccess -> Comp Field
inputField Public = VarFI <$> freshInputVar Public ReadField WriteField 1
inputField Private = VarFP <$> freshInputVar Private ReadField WriteField 1

-- | Requests a fresh 'Boolean' input variable
inputBool :: InputAccess -> Comp Boolean
inputBool Public = VarBI <$> freshInputVar Public ReadBool WriteBool 1
inputBool Private = VarBP <$> freshInputVar Private ReadBool WriteBool 1

-- | Requests a fresh 'UInt' input variable of some bit width
inputUInt :: forall w. (KnownNat w) => InputAccess -> Comp (UInt w)
inputUInt acc = case acc of
  Public -> VarUI <$> freshInputVar acc (ReadUInt width) (WriteUInt width) 1
  Private -> VarUP <$> freshInputVar acc (ReadUInt width) (WriteUInt width) 1
  where
    width = fromIntegral (natVal (Proxy :: Proxy w))

-- | Requests a fresh 'Field' variable
freshVarField :: Comp Field
freshVarField = freshVar

-- | Requests a fresh 'Boolean' variable
freshVarBool :: Comp Boolean
freshVarBool = freshVar

-- | Requests a fresh 'UInt' variable of some bit width
freshVarUInt :: (KnownNat w) => Comp (UInt w)
freshVarUInt = freshVar

--------------------------------------------------------------------------------

-- | Requests a 2D-array of fresh input variables
inputList2 :: (Input t) => InputAccess -> Int -> Int -> Comp [[t]]
inputList2 acc sizeM sizeN = replicateM sizeM (inputList acc sizeN)

-- | Requests a 3D-array of fresh input variables
inputList3 :: (Input t) => InputAccess -> Int -> Int -> Int -> Comp [[[t]]]
inputList3 acc sizeM sizeN sizeO = replicateM sizeM (inputList2 acc sizeN sizeO)

--------------------------------------------------------------------------------

-- | Vector version of 'inputList'
inputVec :: (Input t) => InputAccess -> Int -> Comp (Vector t)
inputVec acc size = Vec.fromList <$> inputList acc size

-- | Vector version of 'inputList2'
inputVec2 :: (Input t) => InputAccess -> Int -> Int -> Comp (Vector (Vector t))
inputVec2 acc sizeM sizeN = Vec.fromList <$> replicateM sizeM (inputVec acc sizeN)

-- | Vector version of 'inputList3'
inputVec3 :: (Input t) => InputAccess -> Int -> Int -> Int -> Comp (Vector (Vector (Vector t)))
inputVec3 acc sizeM sizeN sizeO = Vec.fromList <$> replicateM sizeM (inputVec2 acc sizeN sizeO)

--------------------------------------------------------------------------------

-- | Convert a mutable array to a Haskell list
freeze :: (Mutable t) => ArrM t -> Comp [t]
freeze = fromArrayM

-- | Convert a mutable 2D-array to a list of lists
freeze2 :: (Mutable t) => ArrM (ArrM t) -> Comp [[t]]
freeze2 xs = do
  xs' <- fromArrayM xs
  mapM freeze xs'

-- | Convert a mutable 3D-array to a list of lists of lists
freeze3 :: (Mutable t) => ArrM (ArrM (ArrM t)) -> Comp [[[t]]]
freeze3 xs = do
  xs' <- fromArrayM xs
  mapM freeze2 xs'

-- | Convert a Haskell list to a mutable array
thaw :: (Mutable t) => [t] -> Comp (ArrM t)
thaw = toArrayM

-- | Convert a list of lists to a mutable 2D-array
thaw2 :: (Mutable t) => [[t]] -> Comp (ArrM (ArrM t))
thaw2 xs = mapM thaw xs >>= toArrayM

-- | Convert a list of lists of lists to a mutable 3D-array
thaw3 :: (Mutable t) => [[[t]]] -> Comp (ArrM (ArrM (ArrM t)))
thaw3 xs = mapM thaw2 xs >>= toArrayM

--------------------------------------------------------------------------------

-- | Typeclass for retrieving the element of an array
class Mutable t where
  -- | Allocates a fresh variable for a value
  alloc :: t -> Comp Var

  typeOf :: t -> ElemType

  constructElement :: ElemType -> Addr -> t

instance Mutable Field where
  alloc (VarF var) = return var
  alloc val = assignF val

  typeOf _ = ElemF

  constructElement ElemF elemAddr = VarF elemAddr
  constructElement _ _ = error "expecting element to be of Num"

instance Mutable Boolean where
  alloc (VarB var) = return var
  alloc val = assignB val

  typeOf _ = ElemB

  constructElement ElemB elemAddr = VarB elemAddr
  constructElement _ _ = error "expecting element to be of Bool"

instance (KnownNat w) => Mutable (UInt w) where
  alloc (VarU var) = return var
  alloc val = assignU val

  typeOf val = ElemU (widthOf val)

  constructElement (ElemU _) elemAddr = VarU elemAddr
  constructElement _ _ = error "expecting element to be of UInt"

instance (Mutable ref) => Mutable (ArrM ref) where
  alloc xs@((ArrayRef elemType len _)) = do
    elements <- mapM (accessM xs) [0 .. len - 1]
    fst <$> allocArray elemType elements

  typeOf ((ArrayRef elemType len _)) = ElemArr elemType len

  constructElement (ElemArr l k) elemAddr = ArrayRef l k elemAddr
  constructElement EmptyArr elemAddr = ArrayRef EmptyArr 0 elemAddr
  constructElement _ _ = error "expecting element to be array"

-- | Converts a list of values to an 1D-array
toArrayM :: (Mutable t) => [t] -> Comp (ArrM t)
toArrayM xs = do
  if null xs
    then snd <$> allocArray EmptyArr xs
    else
      let kind = typeOf (head xs)
       in snd <$> allocArray kind xs

-- | Convert an array into a list of expressions
fromArrayM :: (Mutable t) => ArrM t -> Comp [t]
fromArrayM ((ArrayRef _ _ addr)) = readHeapArray addr

-- | Access an element from a 1-D array
accessM :: (Mutable t) => ArrM t -> Int -> Comp t
accessM ((ArrayRef _ _ addr)) i = readHeap (addr, i)

-- | Access an element from a 2-D array
accessM2 :: (Mutable t) => ArrM (ArrM t) -> (Int, Int) -> Comp t
accessM2 addr (i, j) = accessM addr i >>= flip accessM j

-- | Access an element from a 3-D array
accessM3 :: (Mutable t) => ArrM (ArrM (ArrM t)) -> (Int, Int, Int) -> Comp t
accessM3 addr (i, j, k) = accessM addr i >>= flip accessM j >>= flip accessM k

-- | Update an entry of an array.
updateM :: (Mutable t) => ArrM t -> Int -> t -> Comp ()
updateM (ArrayRef elemType _ addr) i expr = do
  var <- alloc expr
  writeHeap addr elemType (i, var)

--------------------------------------------------------------------------------

-- | Internal helper function for allocating an array with values
allocArray :: (Mutable t) => ElemType -> [t] -> Comp (Addr, ArrM u)
allocArray elemType vals = do
  -- allocate a new array for holding the variables of these elements
  addr <- gets compAddrSize
  modify (\st -> st {compAddrSize = succ addr})
  -- allocate new variables for each element
  addresses <- mapM alloc vals
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
readHeap :: (Mutable t) => (Addr, Int) -> Comp t
readHeap (addr, i) = do
  heap <- gets compHeap
  case IntMap.lookup addr heap of
    Nothing -> error "readHeap: address not found"
    Just (elemType, array) -> case IntMap.lookup i array of
      Nothing -> throwError $ IndexOutOfBoundsError addr i array
      Just var -> return $ constructElement elemType var

-- | Internal helper function for accessing an array on the heap
readHeapArray :: (Mutable t) => Addr -> Comp [t]
readHeapArray addr = do
  heap <- gets compHeap
  case IntMap.lookup addr heap of
    Nothing -> error "readHeap: address not found"
    Just (elemType, array) -> return $ map (constructElement elemType) (IntMap.elems array)

--------------------------------------------------------------------------------

-- | An alternative to 'foldM'
reduce :: (Foldable m) => t -> m a -> (t -> a -> Comp t) -> Comp t
reduce a xs f = foldM f a xs

-- | Map with index, basically @mapi@ in OCaml.
mapI :: (Traversable f) => (Int -> a -> b) -> f a -> f b
mapI f = snd . mapAccumL (\i x -> (i + 1, f i x)) 0

--------------------------------------------------------------------------------

-- | Assert that the given expression evaluates to 'true'.
--
--   Assertions play a central role in Keelung, as Keelung is all about constraints between variables.
--
--   /Example/
--
--   Consider the following program that takes two inputs and asserts that the second input is the square of the first:
--
--   @
-- square :: Comp ()
-- square = do
--     x <- input Public
--     y <- input Public
--     -- assert that \'y\' is the square of \'x\'
--     assert (y `eq` (x * x))
--   @
--
--   @since 0.1.0.0
assert :: Boolean -> Comp ()
assert expr = modify' $ \st -> st {compAssertions = expr : compAssertions st}

--------------------------------------------------------------------------------

-- | Allow an expression to be referenced and reused in the future
class Reusable t where
  reuse :: t -> Comp t

instance Reusable Boolean where
  reuse val = do
    var <- assignB val
    return (VarB var)

instance Reusable Field where
  reuse val = do
    var <- assignF val
    return (VarF var)

instance (KnownNat w) => Reusable (UInt w) where
  reuse val = do
    var <- assignU val
    return (VarU var)

instance (Reusable t, Mutable t) => Reusable (ArrM t) where
  reuse = return

instance (Reusable t, Traversable f) => Reusable (f t) where
  reuse = mapM reuse

-- | Allocate a fresh Field variable and assign it to the given expression.
assignF :: Field -> Comp Var
assignF expr = do
  var <- freshVarF
  modify' $ \st -> st {compSideEffects = compSideEffects st :|> AssignmentF var expr}
  return var

-- | Allocate a fresh Boolean variable and assign it to the given expression.
assignB :: Boolean -> Comp Var
assignB expr = do
  var <- freshVarB
  modify' $ \st -> st {compSideEffects = compSideEffects st :|> AssignmentB var expr}
  return var

-- | Allocate a fresh UInt variable and assign it to the given expression.
assignU :: (KnownNat w) => UInt w -> Comp Var
assignU expr = do
  heap <- gets compHeap
  let encoded = runHeapM heap (encode' expr)
  let width = widthOf expr
  var <- freshVarU width
  modify' $ \st -> st {compSideEffects = compSideEffects st :|> AssignmentU width var encoded}
  return var

--------------------------------------------------------------------------------
-- Asserting DivMod relations
--------------------------------------------------------------------------------

-- | Computes the quotient and remainder of two 'UInt' arguments: the dividend and the divisor.
--
--   Note that because 'performDivMod' is a statement, it can only be executed in the 'Comp' context, as shown in the example below:
--
--   /Example/
--
--   @
-- program :: Comp (UInt 32)
-- program = do
--     dividend <- input Public
--     divisor <- input Public
--     (quotient, remainder) <- performDivMod dividend divisor
--     return quotient
--   @
--
--   @since 0.8.3.0
performDivMod ::
  forall w.
  (KnownNat w) =>
  -- | The dividend
  UInt w ->
  -- | The devisor
  UInt w ->
  -- | The quotient and remainder
  Comp (UInt w, UInt w)
performDivMod dividend divisor = do
  remainder <- freshVarU width
  quotient <- freshVarU width
  assertDivMod dividend divisor (VarU quotient) (VarU remainder)
  return (VarU quotient, VarU remainder)
  where
    width = fromIntegral (natVal (Proxy :: Proxy w))

-- | Instead of computing the quotient and remainder from the dividend and divisor with 'performDivMod',
--   we can enforce a relation between the dividend, divisor, quotient, and remainder in Keelung.
--
--   For example, we can enforce the dividend to be an even number and obtain the quotient at
--   the same time, as shown below:
--
--   /Example/
--
--   @
-- assertEven :: UInt 32 -> Comp (UInt 32)
-- assertEven dividend = do
--     quotient <- freshVarUInt
--     assertDivMod dividend 2 quotient 0
--     return quotient
--   @
--
--   @since 0.8.3.0
assertDivMod ::
  forall w.
  (KnownNat w) =>
  -- | The dividend
  UInt w ->
  -- | The divisor
  UInt w ->
  -- | The quotient
  UInt w ->
  -- | The remainder
  UInt w ->
  Comp ()
assertDivMod dividend divisor quotient remainder = do
  heap <- gets compHeap
  let encoded = runHeapM heap $ DivMod width <$> encode' dividend <*> encode' divisor <*> encode' quotient <*> encode' remainder
  modify' (\st -> st {compSideEffects = compSideEffects st :|> encoded})
  where
    width = fromIntegral (natVal (Proxy :: Proxy w))

-- | Computes carry-less quotient and remainder of two 'UInt' arguments: the dividend and the divisor.
--
--   Note that because 'performCLDivMod' is a statement, it can only be executed in the 'Comp' context, as shown in the example below:
--
--   /Example/
--
--   @
-- program :: Comp (UInt 32)
-- program = do
--     dividend <- input Public
--     divisor <- input Public
--     (quotient, remainder) <- performCLDivMod dividend divisor
--     return quotient
--   @
--
--   @since 0.17.0
performCLDivMod ::
  forall w.
  (KnownNat w) =>
  -- | The dividend
  UInt w ->
  -- | The devisor
  UInt w ->
  -- | The quotient and remainder
  Comp (UInt w, UInt w)
performCLDivMod dividend divisor = do
  remainder <- freshVarU width
  quotient <- freshVarU width
  assertCLDivMod dividend divisor (VarU quotient) (VarU remainder)
  return (VarU quotient, VarU remainder)
  where
    width = fromIntegral (natVal (Proxy :: Proxy w))

-- | Instead of computing the carry-less quotient and remainder from the dividend and divisor with 'performCLDivMod',
--   we can enforce a relation between the dividend, divisor, quotient, and remainder in Keelung.
--
--   For example, we can enforce the dividend to be an even number and obtain the quotient at
--   the same time, as shown below:
--
--   /Example/
--
--   @
-- assertEven :: UInt 32 -> Comp (UInt 32)
-- assertEven dividend = do
--     quotient <- freshVarUInt
--     assertCLDivMod dividend 2 quotient 0
--     return quotient
--   @
--
--   @since 0.17.0
assertCLDivMod ::
  forall w.
  (KnownNat w) =>
  -- | The dividend
  UInt w ->
  -- | The divisor
  UInt w ->
  -- | The quotient
  UInt w ->
  -- | The remainder
  UInt w ->
  Comp ()
assertCLDivMod dividend divisor quotient remainder = do
  heap <- gets compHeap
  let encoded = runHeapM heap $ CLDivMod width <$> encode' dividend <*> encode' divisor <*> encode' quotient <*> encode' remainder
  modify' (\st -> st {compSideEffects = compSideEffects st :|> encoded})
  where
    width = fromIntegral (natVal (Proxy :: Proxy w))

--------------------------------------------------------------------------------

-- | Assert that a 'UInt' is lesser than or equal to a given bound.
--
--   /Example/
--
--   @
-- assertLTE3 :: Comp ()
-- assertLTE3 = do
--     x <- inputUInt Public
--     assertLTE x 3
--   @
--
--   @since 0.9.4.0
assertLTE :: (KnownNat w) => UInt w -> Integer -> Comp ()
assertLTE value bound = do
  heap <- gets compHeap
  let width = widthOf value
  let encoded = runHeapM heap $ AssertLTE width <$> encode' value <*> pure bound
  modify' (\st -> st {compSideEffects = compSideEffects st :|> encoded})

-- | Assert that a 'UInt' is lesser than a given bound.
--
--   /Example/
--
--   @
-- assertLT3 :: Comp ()
-- assertLT3 = do
--     x <- inputUInt Public
--     assertLT x 3
--   @
--
--   @since 0.9.5.0
assertLT :: (KnownNat w) => UInt w -> Integer -> Comp ()
assertLT value bound = do
  heap <- gets compHeap
  let width = widthOf value
  let encoded = runHeapM heap $ AssertLT width <$> encode' value <*> pure bound
  modify' (\st -> st {compSideEffects = compSideEffects st :|> encoded})

-- | Assert that a 'UInt' is greater than or equal to a given bound.
--
--   /Example/
--
--   @
-- assertGTE3 :: Comp ()
-- assertGTE3 = do
--     x <- inputUInt Public
--     assertGTE x 3
--   @
--
--   @since 0.9.5.0
assertGTE :: (KnownNat w) => UInt w -> Integer -> Comp ()
assertGTE value bound = do
  heap <- gets compHeap
  let width = widthOf value
  let encoded = runHeapM heap $ AssertGTE width <$> encode' value <*> pure bound
  modify' (\st -> st {compSideEffects = compSideEffects st :|> encoded})

-- | Assert that a 'UInt' is greater than a given bound.
--
--   /Example/
--
--   @
-- assertGT3 :: Comp ()
-- assertGT3 = do
--     x <- inputUInt Public
--     assertGT x 3
--   @
--
--   @since 0.9.5.0
assertGT :: (KnownNat w) => UInt w -> Integer -> Comp ()
assertGT value bound = do
  heap <- gets compHeap
  let width = widthOf value
  let encoded = runHeapM heap $ AssertGT width <$> encode' value <*> pure bound
  modify' (\st -> st {compSideEffects = compSideEffects st :|> encoded})

-- | Convert a 'Field' to a 'UInt'.
--
--   /Example/
--
--   @
-- example :: Comp (UInt 8)
-- example = do
--     x <- inputField Public
--     fromField 8 x
--   @
--
--   @since 0.19.0
fromField :: (KnownNat w) => Width -> Field -> Comp (UInt w)
fromField width exprF = do
  varF <- assignF exprF
  varU <- freshVarU width
  modify' (\st -> st {compSideEffects = compSideEffects st :|> ToUInt width varU varF})
  return (VarU varU)

{-# WARNING toUInt "will be replaced by `fromField` after v0.23" #-}
toUInt :: (KnownNat w) => Width -> Field -> Comp (UInt w)
toUInt = fromField

-- | Convert a 'UInt' to a 'Field'.
--
--   /Example/
--
--   @
-- example :: Comp Field
-- example = do
--     x <- inputUInt @8 Public
--     toField x
--   @
--
--   @since 0.19.0
toField :: (KnownNat w) => UInt w -> Comp Field
toField exprU = do
  varU <- assignU exprU
  varF <- freshVarF
  modify' (\st -> st {compSideEffects = compSideEffects st :|> ToField (widthOf exprU) varU varF})
  return (VarF varF)

-- | Converting a list of 'Boolean' to a 'UInt', ordered from the least significant bit to the most significant bit.
--   When the length of the list is less than the width of the 'UInt', the remaining bits are filled with 'false'.
--   When the length of the list is greater than the width of the 'UInt', the extra bits are discarded.
--
--   /Example/
--
--   @
-- example :: Comp (UInt 8)
-- example = do
--     b <- inputBool
--     fromBools [true, b, b .&. false, true, false]
--
--   @
--
--   @since 0.19.0
fromBools :: forall w. (KnownNat w) => [Boolean] -> Comp (UInt w)
fromBools bs = do
  -- trim or pad the list of bits to the width of UInt
  let bs' = case length bs `compare` width of
        LT -> Seq.fromList bs <> Seq.replicate (width - length bs) false
        EQ -> Seq.fromList bs
        GT -> Seq.fromList (take width bs)
  varU <- freshVarU width
  modify' (\st -> st {compSideEffects = compSideEffects st :|> BitsToUInt width varU bs'})
  return (VarU varU)
  where
    width = fromIntegral (natVal (Proxy :: Proxy w))

{-# WARNING pack "will be replaced by `fromBools` after v0.23" #-}
pack :: forall w. (KnownNat w) => [Boolean] -> Comp (UInt w)
pack = fromBools

--------------------------------------------------------------------------------

-- | Data type representing the side effects of a computation.
data SideEffect
  = AssignmentF Var Field
  | AssignmentB Var Boolean
  | AssignmentU Width Var Encoding.UInt
  | ToUInt Width Var Var
  | ToField Width Var Var
  | BitsToUInt Width Var (Seq Boolean)
  | DivMod Width Encoding.UInt Encoding.UInt Encoding.UInt Encoding.UInt
  | CLDivMod Width Encoding.UInt Encoding.UInt Encoding.UInt Encoding.UInt
  | AssertLTE Width Encoding.UInt Integer
  | AssertLT Width Encoding.UInt Integer
  | AssertGTE Width Encoding.UInt Integer
  | AssertGT Width Encoding.UInt Integer
  deriving (Show, Eq)
