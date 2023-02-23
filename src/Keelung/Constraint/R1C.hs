{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Keelung.Constraint.R1C (R1C (..), satisfy, freeVars) where

import Control.DeepSeq (NFData)
import Data.Field.Galois (GaloisField)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet (IntSet)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Keelung.Data.Polynomial (Poly)
import Keelung.Data.Polynomial qualified as Poly

--------------------------------------------------------------------------------

-- | A Rank-1 Constraint is a relation between 3 polynomials
--      Ax * Bx = Cx
data R1C n = R1C (Either n (Poly n)) (Either n (Poly n)) (Either n (Poly n))
  deriving (Generic, NFData)

instance (Eq n, Num n) => Eq (R1C n) where
  R1C a b c == R1C a' b' c' =
    -- if the RHS are the same
    (c == c' && (a == a' && b == b' || a == b' && b == a'))
      -- if the RHS are the negation of each other
      || ( negate' c == c'
             && ( negate' a == a' && b == b'
                    || a == a' && negate' b == b'
                    || negate' a == b' && b == a'
                    || a == b' && negate' b == a'
                )
         )
    where
      negate' (Left n) = Left (-n)
      negate' (Right p) = Right (Poly.negate p)

instance Functor R1C where
  fmap f (R1C a b c) = R1C (fmapE a) (fmapE b) (fmapE c)
    where
      fmapE (Left x) = Left (f x)
      fmapE (Right xs) = Right (fmap f xs)

instance Serialize n => Serialize (R1C n)

instance (Num n, Eq n, Ord n) => Ord (R1C n) where
  compare x@(R1C a b c) y@(R1C e f g) = case (isRank1 x, isRank1 y) of
    (True, False) -> LT
    (False, True) -> GT
    (True, True) ->
      -- both are of rank 1
      -- the one with a constant term on the RHS is considered smaller
      case (c, g) of
        (Left _, Right _) -> LT
        (Right _, Left _) -> GT
        (Left c0, Left c1) -> compare c0 c1 -- compare the constant terms
        (Right v0, Right v1) -> compare v0 v1 -- compare the polynomials
    (False, False) ->
      -- both are of rank 2
      compare (a, b, c) (e, f, g)

instance (Show n, Ord n, Eq n, Num n) => Show (R1C n) where
  show (R1C aX bX cX) = case (aX, bX, cX) of
    (Left 0, _, _) -> "0 = " ++ showVec cX
    (_, Left 0, _) -> "0 = " ++ showVec cX
    (Left 1, _, _) -> showVec bX ++ " = " ++ showVec cX
    (_, Left 1, _) -> showVec aX ++ " = " ++ showVec cX
    (_, _, _) -> showVecWithParen aX ++ " * " ++ showVecWithParen bX ++ " = " ++ showVec cX
    where
      showVec :: (Show n, Ord n, Eq n, Num n) => Either n (Poly n) -> String
      showVec (Left c) = show c
      showVec (Right xs) = show xs

      -- wrap the string with parenthesis if it has more than 1 term
      showVecWithParen :: (Show n, Ord n, Eq n, Num n) => Either n (Poly n) -> String
      showVecWithParen (Left c) = showVec (Left c) -- no parenthesis
      showVecWithParen (Right xs) =
        let termNumber =
              IntMap.size (Poly.coeffs xs)
                + if Poly.constant xs == 0
                  then 0
                  else 1
         in if termNumber < 2
              then showVec (Right xs)
              else "(" ++ showVec (Right xs) ++ ")"

-- | See if a R1C is satified by a given assignment
satisfy :: GaloisField a => R1C a -> IntMap a -> Bool
satisfy constraint assignment
  | R1C aV bV cV <- constraint =
      evaluate aV assignment * evaluate bV assignment == evaluate cV assignment
  where
    evaluate :: GaloisField a => Either a (Poly a) -> IntMap a -> a
    evaluate (Left x) _ = x
    evaluate (Right p) w = Poly.evaluate p w

-- | Free variables in a R1C
freeVars :: R1C n -> IntSet
freeVars (R1C a b c) = freeVarsE a <> freeVarsE b <> freeVarsE c
  where
    freeVarsE (Left _) = mempty
    freeVarsE (Right p) = Poly.vars p

-- | An R1C is of rank 1 if either side of the multiplication is a constant
isRank1 :: R1C n -> Bool
isRank1 (R1C a b _) = isConstant a || isConstant b
  where
    isConstant (Left _) = True
    isConstant (Right _) = False