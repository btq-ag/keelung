{-# LANGUAGE DataKinds #-}

module Keelung.Monad where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Keelung.Error
import Keelung.Syntax

--------------------------------------------------------------------------------

-- | A Heap is an mapping of mappings of variables
type Heap = IntMap (IntMap Int)

--------------------------------------------------------------------------------

-- | An Assignment associates an expression with a reference
data Assignment ty n = Assignment (Ref ('V ty)) (Expr ty n)

instance Show n => Show (Assignment ty n) where
  show (Assignment var expr) = show var <> " := " <> show expr

instance Functor (Assignment ty) where
  fmap f (Assignment var expr) = Assignment var (fmap f expr)

--------------------------------------------------------------------------------

-- | Bookkeeping for the elaboration of Keelung programs.
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

--------------------------------------------------------------------------------

-- The monad for elaborating Keelung programs.
type Comp n = StateT (Computation n) (Except Error)
