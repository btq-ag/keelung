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
import Keelung.Syntax.Counters
import Keelung.Types

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
  | EqF Field Field
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
    EqF x y -> showParen (prec > 5) $ showsPrec 6 x . showString " = " . showsPrec 6 y
    EqU _ x y -> showParen (prec > 5) $ showsPrec 6 x . showString " = " . showsPrec 6 y
    BitU _ x i -> showParen (prec > 6) $ showsPrec 7 x . showString " [" . shows i . showString "]"

--------------------------------------------------------------------------------

data Field
  = ValF Integer
  | ValFR Rational
  | VarF Var
  | VarFI Var
  | AddF Field Field
  | SubF Field Field
  | MulF Field Field
  | DivF Field Field
  | IfF Boolean Field Field
  | BtoF Boolean
  deriving (Generic, Eq, NFData)

instance Serialize Field

instance Show Field where
  showsPrec prec expr = case expr of
    ValF n -> shows n
    ValFR n -> shows n
    VarF var -> showString "$F" . shows var
    VarFI var -> showString "$F" . shows var
    AddF x y -> showParen (prec > 6) $ showsPrec 6 x . showString " + " . showsPrec 7 y
    SubF x y -> showParen (prec > 6) $ showsPrec 6 x . showString " - " . showsPrec 7 y
    MulF x y -> showParen (prec > 7) $ showsPrec 7 x . showString " * " . showsPrec 8 y
    DivF x y -> showParen (prec > 7) $ showsPrec 7 x . showString " / " . showsPrec 8 y
    IfF p x y -> showParen (prec > 1) $ showString "if " . showsPrec 2 p . showString " then " . showsPrec 2 x . showString " else " . showsPrec 2 y
    BtoF x -> showString "B→F " . showsPrec prec x

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
  | ShLU Width Int UInt
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
    ShLU _ n x -> showParen (prec > 8) $ showString "ShL " . showsPrec 9 n . showString " " . showsPrec 9 x
    IfU _ p x y -> showParen (prec > 1) $ showString "if " . showsPrec 2 p . showString " then " . showsPrec 2 x . showString " else " . showsPrec 2 y
    BtoU _ x -> showString "B→U " . showsPrec prec x

--------------------------------------------------------------------------------

data Expr
  = Unit
  | Boolean Boolean
  | Field Field
  | UInt UInt
  | Array (Array Int Expr)
  deriving (Generic, Eq, NFData)

instance Show Expr where
  showsPrec prec expr = case expr of
    Unit -> showString "()"
    Boolean bool -> showsPrec prec bool
    Field num -> showsPrec prec num
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

prettyList3 :: Int -> [String] -> String
prettyList3 n list = case list of
  [] -> ""
  [x] -> "[" <> x <> "]\n"
  (x : xs) ->
    unlines $
      map (replicate n ' ' <>) $
        "" : "[ " <> x : map (", " <>) xs <> ["]\n"]

--------------------------------------------------------------------------------

-- | An Assignment associates an expression with a reference
data Assignment
  = AssignmentB Var Boolean
  | AssignmentBI Var Boolean
  | AssignmentF Var Field
  | AssignmentFI Var Field
  | AssignmentU Width Var UInt
  | AssignmentUI Width Var UInt
  deriving (Generic, NFData)

instance Show Assignment where
  show (AssignmentB var bool) = "$" <> show var <> " := " <> show bool
  show (AssignmentBI var bool) = "$" <> show var <> " := " <> show bool
  show (AssignmentF var num) = "$" <> show var <> " := " <> show num
  show (AssignmentFI var num) = "$" <> show var <> " := " <> show num
  show (AssignmentU _ var uint) = "$" <> show var <> " := " <> show uint
  show (AssignmentUI _ var uint) = "$" <> show var <> " := " <> show uint

instance Serialize Assignment

--------------------------------------------------------------------------------

-- | Data structure for elaboration bookkeeping
data Computation = Computation
  { -- Variable bookkeeping
    compCounters :: !Counters,
    -- Assignments
    compAssignmentF :: IntMap Field,
    compAssignmentFI :: IntMap Field,
    compAssignmentB :: IntMap Boolean,
    compAssignmentBI :: IntMap Boolean,
    compAssignmentU :: IntMap (IntMap UInt),
    compAssignmentUI :: IntMap (IntMap UInt),
    -- Assertions are expressions that are expected to be true
    compAssertions :: [Expr]
  }
  deriving (Generic, NFData)

instance Show Computation where
  show (Computation _ aF aFI aB aBI aU aUI assertions) =
    "{\n\n"
      <> "  Field assignments: "
      <> prettyList3 8 (map (\(var, val) -> "$F" <> show var <> " := " <> show val) $ IntMap.toList aF)
      <> prettyList3 8 (map (\(var, val) -> "$FI" <> show var <> " := " <> show val) $ IntMap.toList aFI)
      <> "  Boolean assignments: "
      <> prettyList3 8 (map (\(var, val) -> "$B" <> show var <> " := " <> show val) $ IntMap.toList aB)
      <> prettyList3 8 (map (\(var, val) -> "$BI" <> show var <> " := " <> show val) $ IntMap.toList aBI)
      <> "  Unsigned Int assignments: "
      <> prettyUIntAssignments "$U" aU
      <> prettyUIntAssignments "$UI" aUI
      <> "  assertions: "
      <> prettyList2 8 assertions
      <> "\n}"
    where
      -- prettyUInts :: IntMap (IntMap UInt) -> String
      prettyUIntAssignments :: String -> IntMap (IntMap UInt) -> String
      prettyUIntAssignments prefix assignments =
        prettyList3 8 $
          concatMap
            ( \(width, xs) ->
                map (\(var, val) -> prefix <> toSubscript width <> show var <> " := " <> show val) $
                  IntMap.toList xs
            )
            $ IntMap.toList assignments

      toSubscript :: Int -> String
      toSubscript = map go . show
        where
          go c = case c of
            '0' -> '₀'
            '1' -> '₁'
            '2' -> '₂'
            '3' -> '₃'
            '4' -> '₄'
            '5' -> '₅'
            '6' -> '₆'
            '7' -> '₇'
            '8' -> '₈'
            '9' -> '₉'
            _ -> c

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
  (expr, comp') <- left show $ runComp (Computation mempty mempty mempty mempty mempty mempty mempty mempty) prog
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

assignF :: Var -> Field -> Comp ()
assignF var e = modify' $ \st -> st {compAssignmentF = IntMap.insert var e (compAssignmentF st)}

assignFI :: Var -> Field -> Comp ()
assignFI var e = modify' $ \st -> st {compAssignmentFI = IntMap.insert var e (compAssignmentFI st)}

assignB :: Var -> Boolean -> Comp ()
assignB var e = modify' $ \st -> st {compAssignmentB = IntMap.insert var e (compAssignmentB st)}

assignBI :: Var -> Boolean -> Comp ()
assignBI var e = modify' $ \st -> st {compAssignmentBI = IntMap.insert var e (compAssignmentBI st)}

assignU :: Width -> Var -> UInt -> Comp ()
assignU w var e = modify' $ \st -> st {compAssignmentU = IntMap.insertWith (<>) w (IntMap.singleton var e) (compAssignmentU st)}

assignUI :: Width -> Var -> UInt -> Comp ()
assignUI w var e = modify' $ \st -> st {compAssignmentUI = IntMap.insertWith (<>) w (IntMap.singleton var e) (compAssignmentUI st)}