{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Keelung.Constraint.R1C where

import Control.DeepSeq (NFData)
import Data.Field.Galois (GaloisField)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Keelung.Constraint.Polynomial (Poly)
import qualified Keelung.Constraint.Polynomial as Poly
import Keelung.Field.N (N (N))

--------------------------------------------------------------------------------

-- | A Rank-1 Constraint is a relation between 3 polynomials
--      Ax * Bx = Cx
data R1C n = R1C (Either n (Poly n)) (Either n (Poly n)) (Either n (Poly n))
  deriving (Eq, Ord, Generic, NFData)

instance Functor R1C where
  fmap f (R1C a b c) = R1C (fmapE a) (fmapE b) (fmapE c)
    where
      fmapE (Left x) = Left (f x)
      fmapE (Right xs) = Right (fmap f xs)

instance Serialize n => Serialize (R1C n)

instance (GaloisField n, Integral n) => Show (R1C n) where
  show (R1C aX bX cX) = case (aX, bX, cX) of
    (Left 0, _, _) -> "0 = " ++ showVec cX
    (_, Left 0, _) -> "0 = " ++ showVec cX
    (Left 1, _, _) -> showVec bX ++ " = " ++ showVec cX
    (_, Left 1, _) -> showVec aX ++ " = " ++ showVec cX
    (_, _, _) -> showVecWithParen aX ++ " * " ++ showVecWithParen bX ++ " = " ++ showVec cX
    where
      showVec :: (GaloisField n, Integral n) => Either n (Poly n) -> String
      showVec (Left c) = show (N c)
      showVec (Right xs) = show xs

      -- wrap the string with parenthesis if it has more than 1 term
      showVecWithParen :: (GaloisField n, Integral n) => Either n (Poly n) -> String
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
