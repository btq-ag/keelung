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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

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
    Inputable (..),
    Pub,
    Prv,
    getVar,
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
import Keelung.Syntax.Encode (encode', runHeapM, Encode(..))
import Keelung.Syntax.Encode.Syntax qualified as Encoding
import Data.Kind

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

-- -- | TODO: need a chosing scheme for input datatype?
-- --   pre-determine "shape" of type at compile time
-- --     -> less flexibility, better optimization
-- --   Is it possible to have any types as inputs?
-- class GInput (f :: Type -> Type) where
--   -- the first f x is for schema only, showing how
--   -- the input should be deconstructed when there are
--   -- more than one ways.
--   ginput :: f x -> InputAccess -> Comp (f x)
-- 
-- instance GInput U1 where
--   ginput U1 _ = return U1
-- 
-- instance (GInput a, GInput b) => GInput (a :+: b) where
--   ginput (L1 res) acc = L1 <$> ginput res acc
--   ginput (R1 res) acc = R1 <$> ginput res acc
-- 
-- -- flatten all elements into 1-d array
-- instance (GInput a, GInput b) => GInput (a :*: b) where
--   ginput (l :*: r) acc = do
--     a' <- ginput l acc
--     b' <- ginput r acc
--     return (a' :*: b')
-- 
-- instance (GInput a) => GInput (M1 i c a) where
--   ginput (M1 x) acc = ginput x acc >>= \y -> return (M1 y)
-- 
-- instance (Input a) => GInput (K1 i a) where
--   ginput (K1 x) acc = inputs x acc >>= \y -> return (K1 y)
-- 
-- | Typeclass for operations on base types
class Input t where
  -- | Request a fresh input variable
  --
  --   @since 0.1.0.0
  input :: InputAccess -> Comp t

-- input :: (Input t) => InputAccess -> Comp t
-- input = inputs (error "This type needs a dummy value to represent its input structure.")

---- // Experimenting Generalized Datatype as Inputs

data KVar :: (InputAccess -> Type -> Type) where
  PubVar :: t -> KVar 'Public t
  PrvVar :: t -> KVar 'Private t

type Pub t = KVar 'Public t
type Prv t = KVar 'Private t

getVar :: KVar i t -> t
getVar (PubVar a) = a
getVar (PrvVar a) = a

-- | functors for KVar's representation
data IsPub :: Type -> Type -> Type where IsPub :: t -> IsPub t a
data IsPrv :: Type -> Type -> Type where IsPrv :: t -> IsPrv t a

instance Encode t => Encode (KVar i t) where
  encode (PubVar a) = encode a
  encode (PrvVar a) = encode a

instance Generic (KVar 'Public t) where
  type Rep (KVar 'Public t) = IsPub t
  from (PubVar t) = IsPub t
  to (IsPub t) = PubVar t

instance Generic (KVar 'Private t) where
  type Rep (KVar 'Private t) = IsPrv t
  from (PrvVar t) = IsPrv t
  to (IsPrv t) = PrvVar t

class GInputable f where
  ginput :: Comp (f x)

instance Inputable t => GInputable (IsPub t) where
  ginput = do
    a <- input' :: Comp t
    return (from $ PubVar a)

instance Inputable t => GInputable (IsPrv t) where
  ginput = do
    a <- input' :: Comp t
    return (from $ PrvVar a)

instance GInputable U1 where
  ginput = return U1

-- flatten all elements into 1-d array
instance (GInputable a, GInputable b) => GInputable (a :*: b) where
  ginput = do
    a' <- ginput
    b' <- ginput
    return (a' :*: b')

instance (GInputable a) => GInputable (M1 i c a) where
  ginput = M1 <$> ginput

instance (Inputable a) => GInputable (K1 i a) where
  ginput = K1 <$> input'

-- | class for types that are "inputable", i.e., products that contains only base types.
--   Conditions for inputables:
--   1. Only product types allowed
--   2. Every base field must contains accessibility info
class Inputable a where
  input' :: Comp a
  default input' :: (Generic a, GInputable (Rep a)) => Comp a
  input' = to <$> ginput

instance Inputable (KVar 'Public Field) where
  input' = PubVar <$> inputField Public

instance Inputable (KVar 'Private Field) where
  input' = PrvVar <$> inputField Public

instance Inputable (KVar 'Public Boolean) where
  input' = PubVar <$> inputBool Public

instance Inputable (KVar 'Private Boolean) where
  input' = PrvVar <$> inputBool Private

instance (KnownNat w) => Inputable (KVar 'Public (UInt w)) where
  input' = PubVar <$> inputUInt Public

instance (KnownNat w) => Inputable (KVar 'Private (UInt w)) where
  input' = PrvVar <$> inputUInt Private

----


-- | TODO: modify input to not have a choosing scheme - users can decide how it's constructed.
--   FOR NOW Encoding is not one-to-one, must be fixed before Decoding is possible.
-- decodeInput :: [Field] -> t

class Proper t where
  -- | Request a fresh variable
  --
  --   @since 0.8.4.0
  freshVar :: Comp t

  -- | Request a list of fresh input variables
  --   default implementation simply applies `replicateM` on `input`
  -- inputList :: InputAccess -> Int -> Comp [t]
  -- inputList acc size = replicateM size $ input _ acc

  -- | Conditional clause
  --
  --   @since 0.1.0.0
  cond :: Boolean -> t -> t -> t


instance Input Field where
  input = inputField

  -- \| Specialized implementation for Field
  -- inputList acc len = do
  --   start <- freshInputVar acc ReadField WriteField len
  --   return $ case acc of
  --     Public -> map VarFI [start .. start + len - 1]
  --     Private -> map VarFP [start .. start + len - 1]

instance Proper Field where
  freshVar = VarF <$> freshVarF

  cond = IfF

instance Input Boolean where
  input = inputBool

instance Proper Boolean where
  -- \| Specialized implementation for Boolean
  -- inputList acc len = do
  --   start <- freshInputVar acc ReadBool WriteBool len
  --   return $ case acc of
  --     Public -> map VarBI [start .. start + len - 1]
  --     Private -> map VarBP [start .. start + len - 1]

  freshVar = VarB <$> freshVarB

  cond = IfB

instance (KnownNat w) => Input (UInt w) where
  input = inputUInt

instance (KnownNat w) => Proper (UInt w) where
  -- \| Specialized implementation for UInt
  -- inputList acc len = do
  --   start <- freshInputVar acc (ReadUInt width) (WriteUInt width) len
  --   return $ case acc of
  --     Public -> map VarUI [start .. start + len - 1]
  --     Private -> map VarUP [start .. start + len - 1]
  --   where
  --     width = fromIntegral (natVal (Proxy :: Proxy w))

  freshVar = VarU <$> freshVarU width
    where
      width = fromIntegral (natVal (Proxy :: Proxy w))

  cond = IfU

instance Inputable ()
-- instance (Inputable a) => Inputable [a]
instance (Inputable a) => Inputable (Proxy a)
instance (Inputable a, Inputable b) => Inputable (a, b)
instance (Inputable a, Inputable b, Inputable c) => Inputable (a, b, c)
instance (Inputable a, Inputable b, Inputable c, Inputable d) => Inputable (a, b, c, d)
instance (Inputable a, Inputable b, Inputable c, Inputable d, Inputable e) => Inputable (a, b, c, d, e)
instance (Inputable a, Inputable b, Inputable c, Inputable d, Inputable e, Inputable f) => Inputable (a, b, c, d, e, f)
-- instance (Input t) => Input (Maybe t) where
-- instance (Input a, Input b) => Input (Either a b) where

inputList :: (Input t) => InputAccess -> Int -> Comp [t]
inputList acc len = replicateM len (input acc)
-- instance (Input t) => Input [t] where
--   size = length
-- 
--   input len acc = inputList acc len
-- 
--   inputList = inputList
-- 
--   freshVars = _
-- 
--   cond c = zipWith (cond c)
-- 
-- instance Input a => Input (Maybe a) where
--   size = _

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