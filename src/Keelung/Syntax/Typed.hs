{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Keelung.Syntax.Typed where

import Control.Arrow (left)
import Control.DeepSeq (NFData)
import Control.Monad.Except
import Control.Monad.State
import Data.Array.Unboxed (Array)
import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Keelung.Error (ElabError)
import Keelung.Field (FieldType)
import Keelung.Syntax.VarCounters
import Keelung.Types
import Keelung.Syntax.Counters

type Width = Int

--------------------------------------------------------------------------------

data Boolean
  = ValB Bool
  | VarB Var
  | InputVarB Var
  | AndB Boolean Boolean
  | OrB Boolean Boolean
  | XorB Boolean Boolean
  | NotB Boolean
  | IfB Boolean Boolean Boolean
  | EqB Boolean Boolean
  | EqN Number Number
  | EqU Width UInt UInt
  | BitU Width UInt Int
  deriving (Generic, Eq, NFData)

instance Serialize Boolean

instance Show Boolean where
  showsPrec prec expr = case expr of
    ValB n -> shows n
    VarB var -> showString "$B" . shows var
    InputVarB var -> showString "$B" . shows var
    AndB x y -> showParen (prec > 3) $ showsPrec 4 x . showString " ∧ " . showsPrec 3 y
    OrB x y -> showParen (prec > 2) $ showsPrec 3 x . showString " ∨ " . showsPrec 2 y
    XorB x y -> showParen (prec > 4) $ showsPrec 5 x . showString " ⊕ " . showsPrec 4 y
    NotB x -> showParen (prec > 8) $ showString "¬ " . showsPrec 9 x
    IfB p x y -> showParen (prec > 1) $ showString "if " . showsPrec 2 p . showString " then " . showsPrec 2 x . showString " else " . showsPrec 2 y
    EqB x y -> showParen (prec > 5) $ showsPrec 6 x . showString " = " . showsPrec 6 y
    EqN x y -> showParen (prec > 5) $ showsPrec 6 x . showString " = " . showsPrec 6 y
    EqU _ x y -> showParen (prec > 5) $ showsPrec 6 x . showString " = " . showsPrec 6 y
    BitU _ x i -> showParen (prec > 6) $ showsPrec 7 x . showString " [" . shows i . showString "]"

--------------------------------------------------------------------------------

data Number
  = ValN Integer
  | ValNR Rational
  | VarN Var
  | InputVarN Var
  | AddN Number Number
  | SubN Number Number
  | MulN Number Number
  | DivN Number Number
  | IfN Boolean Number Number
  | BtoN Boolean
  deriving (Generic, Eq, NFData)

instance Serialize Number

instance Show Number where
  showsPrec prec expr = case expr of
    ValN n -> shows n
    ValNR n -> shows n
    VarN var -> showString "$N" . shows var
    InputVarN var -> showString "$N" . shows var
    AddN x y -> showParen (prec > 6) $ showsPrec 6 x . showString " + " . showsPrec 7 y
    SubN x y -> showParen (prec > 6) $ showsPrec 6 x . showString " - " . showsPrec 7 y
    MulN x y -> showParen (prec > 7) $ showsPrec 7 x . showString " * " . showsPrec 8 y
    DivN x y -> showParen (prec > 7) $ showsPrec 7 x . showString " / " . showsPrec 8 y
    IfN p x y -> showParen (prec > 1) $ showString "if " . showsPrec 2 p . showString " then " . showsPrec 2 x . showString " else " . showsPrec 2 y
    BtoN x -> showString "B→N " . showsPrec prec x

--------------------------------------------------------------------------------

data UInt
  = ValU Width Integer
  | VarU Width Var
  | InputVarU Width Var
  | AddU Width UInt UInt
  | SubU Width UInt UInt
  | MulU Width UInt UInt
  | AndU Width UInt UInt
  | OrU Width UInt UInt
  | XorU Width UInt UInt
  | NotU Width UInt
  | RoLU Width Int UInt
  | IfU Width Boolean UInt UInt
  | BtoU Width Boolean
  deriving (Generic, Eq, NFData)

instance Serialize UInt

instance Show UInt where
  showsPrec prec expr = case expr of
    ValU _ n -> shows n
    VarU w var -> showString "$U[" . shows w . showString "]" . shows var
    InputVarU w var -> showString "$U[" . shows w . showString "]" . shows var
    AddU _ x y -> showParen (prec > 6) $ showsPrec 6 x . showString " + " . showsPrec 7 y
    SubU _ x y -> showParen (prec > 6) $ showsPrec 6 x . showString " - " . showsPrec 7 y
    MulU _ x y -> showParen (prec > 7) $ showsPrec 7 x . showString " * " . showsPrec 8 y
    AndU _ x y -> showParen (prec > 3) $ showsPrec 4 x . showString " ∧ " . showsPrec 3 y
    OrU _ x y -> showParen (prec > 2) $ showsPrec 3 x . showString " ∨ " . showsPrec 2 y
    XorU _ x y -> showParen (prec > 4) $ showsPrec 5 x . showString " ⊕ " . showsPrec 4 y
    NotU _ x -> showParen (prec > 8) $ showString "¬ " . showsPrec prec x
    RoLU _ n x -> showParen (prec > 8) $ showString "RoL " . showsPrec 9 n . showString " " . showsPrec 9 x
    IfU _ p x y -> showParen (prec > 1) $ showString "if " . showsPrec 2 p . showString " then " . showsPrec 2 x . showString " else " . showsPrec 2 y
    BtoU _ x -> showString "B→U " . showsPrec prec x

--------------------------------------------------------------------------------

data Expr
  = Unit
  | Boolean Boolean
  | Number Number
  | UInt UInt
  | Array (Array Int Expr)
  deriving (Generic, Eq, NFData)

instance Show Expr where
  showsPrec prec expr = case expr of
    Unit -> showString "()"
    Boolean bool -> showsPrec prec bool
    Number num -> showsPrec prec num
    UInt uint -> showsPrec prec uint
    Array xs -> showList (toList xs)

instance Serialize Expr

instance Serialize FieldType

--------------------------------------------------------------------------------

data Elaborated = Elaborated
  { -- | The resulting 'Expr'
    elabExpr :: !Expr,
    -- | The state of computation after elaboration
    elabComp :: Computation
  }
  deriving (Generic, NFData)

instance Show Elaborated where
  show (Elaborated expr comp) =
    "{\n  expression: "
      <> showExpr
      <> "\n  compuation state: \n"
      <> indent (indent (show comp))
      <> "\n}"
    where
      showExpr = case expr of
        Array xs -> prettyList2 4 (toList xs)
        _ -> show expr

instance Serialize Elaborated

-- | Prettify list of stuff
prettyList :: Show a => [a] -> [String]
prettyList [] = ["[]"]
prettyList [x] = ["[" <> show x <> "]"]
prettyList (x : xs) = "" : "[ " <> show x : map (\y -> ", " <> show y) xs <> ["]"]

prettyList2 :: Show a => Int -> [a] -> String
prettyList2 n list = case list of
  [] -> "[]"
  [x] -> "[" <> show x <> "]"
  (x : xs) ->
    unlines $
      map (replicate n ' ' <>) $
        "" : "[ " <> show x : map (\y -> ", " <> show y) xs <> ["]"]

--------------------------------------------------------------------------------

-- | An Assignment associates an expression with a reference
data Assignment
  = AssignmentB Var Boolean
  | AssignmentBI Var Boolean
  | AssignmentN Var Number
  | AssignmentNI Var Number
  | AssignmentU Width Var UInt
  | AssignmentUI Width Var UInt
  deriving (Generic, NFData)

instance Show Assignment where
  show (AssignmentB var bool) = "$" <> show var <> " := " <> show bool
  show (AssignmentBI var bool) = "$" <> show var <> " := " <> show bool
  show (AssignmentN var num) = "$" <> show var <> " := " <> show num
  show (AssignmentNI var num) = "$" <> show var <> " := " <> show num
  show (AssignmentU _ var uint) = "$" <> show var <> " := " <> show uint
  show (AssignmentUI _ var uint) = "$" <> show var <> " := " <> show uint

instance Serialize Assignment

--------------------------------------------------------------------------------

-- | Data structure for elaboration bookkeeping
data Computation = Computation
  { -- Variable bookkeeping
    compVarCounters :: !VarCounters,
    compCounters :: !Counters,
    -- Assignments
    compNumAsgns :: [Assignment],
    compBoolAsgns :: [Assignment],
    compUIntAsgns :: IntMap [Assignment],
    -- Assertions are expressions that are expected to be true
    compAssertions :: [Expr]
  }
  deriving (Generic, NFData)

instance Show Computation where
  show (Computation varCounters _ numAsgns boolAsgns uintAsgns assertions) =
    "{\n" <> indent (show varCounters)
      <> "\n  Number assignments: "
      <> prettyList2 8 numAsgns
      <> "\n  Boolean assignments: "
      <> prettyList2 8 boolAsgns
      <> "\n  Unsigned Int assignments: "
      <> prettyList2 8 (IntMap.elems uintAsgns)
      <> "\n  assertions: "
      <> prettyList2 8 assertions
      <> "\n\
         \}"

instance Serialize Computation

--------------------------------------------------------------------------------

type Comp = StateT Computation (Except ElabError)

-- | How to run the 'Comp' monad
runComp :: Computation -> Comp a -> Either ElabError (a, Computation)
runComp comp f = runExcept (runStateT f comp)

evalComp :: Computation -> Comp a -> Either ElabError a
evalComp comp f = runExcept (evalStateT f comp)

modifyCounter :: (Counters -> Counters) -> Comp ()
modifyCounter f = modify (\comp -> comp {compCounters = f (compCounters comp)})

elaborate :: Comp Expr -> Either String Elaborated
elaborate prog = do
  (expr, comp') <- left show $ runComp (Computation mempty mempty mempty mempty mempty mempty) prog
  return $ Elaborated expr comp'

-- | Allocate a fresh variable.
allocVarN :: Comp Var
allocVarN = do
  counters <- gets compCounters
  let index = getCount OfIntermediate OfField counters
  modifyCounter $ addCount OfIntermediate OfField 1
  return index

allocVarB :: Comp Var
allocVarB = do
  counters <- gets compCounters
  let index = getCount OfIntermediate OfBoolean counters
  modifyCounter $ addCount OfIntermediate OfBoolean 1
  return index

allocVarU :: Width -> Comp Var
allocVarU width = do
  counters <- gets compCounters
  let index = getCount OfIntermediate (OfUInt width) counters
  modifyCounter $ addCount OfIntermediate (OfUInt width) 1
  return index

assignN :: Var -> Number -> Comp ()
assignN var e = modify' $ \st -> st {compNumAsgns = AssignmentN var e : compNumAsgns st}

assignNI :: Var -> Number -> Comp ()
assignNI var e = modify' $ \st -> st {compNumAsgns = AssignmentNI var e : compNumAsgns st}

assignB :: Var -> Boolean -> Comp ()
assignB var e = modify' $ \st -> st {compBoolAsgns = AssignmentB var e : compBoolAsgns st}

assignBI :: Var -> Boolean -> Comp ()
assignBI var e = modify' $ \st -> st {compBoolAsgns = AssignmentBI var e : compBoolAsgns st}

assignU :: Width -> Var -> UInt -> Comp ()
assignU w var e = modify' $ \st -> st {compUIntAsgns = IntMap.insertWith (<>) w [AssignmentU w var e] (compUIntAsgns st)}

assignUI :: Width -> Var -> UInt -> Comp ()
assignUI w var e = modify' $ \st -> st {compUIntAsgns = IntMap.insertWith (<>) w [AssignmentUI w var e] (compUIntAsgns st)}