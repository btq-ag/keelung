module Keelung.Constraint.R1C where

import Data.Field.Galois (GaloisField)
import Data.IntMap (IntMap)
import Keelung.Constraint.Polynomial (Poly)
import qualified Keelung.Constraint.Polynomial as Poly
import Keelung.Field (N (..))

--------------------------------------------------------------------------------

-- | A Rank-1 Constraint is a relation between 3 polynomials
--      Ax * Bx = Cx
data R1C n = R1C (Either n (Poly n)) (Either n (Poly n)) (Either n (Poly n))
  deriving (Eq)

instance (Show n, Integral n, Bounded n, Fractional n) => Show (R1C n) where
  show (R1C aX bX cX) = case (aX, bX, cX) of
    (Left 0, _, _) -> "0 = " ++ showVec cX
    (_, Left 0, _) -> "0 = " ++ showVec cX
    (Left 1, _, _) -> showVec bX ++ " = " ++ showVec cX
    (_, Left 1, _) -> showVec aX ++ " = " ++ showVec cX
    (_, _, _) -> showVec aX ++ " * " ++ showVec bX ++ " = " ++ showVec cX
    where
      showVec (Left c) = show (N c)
      showVec (Right xs) = show xs

-- | See if a R1C is satified by a given assignment
satisfy :: GaloisField a => R1C a -> IntMap a -> Bool
satisfy constraint assignment
  | R1C aV bV cV <- constraint =
    evaluate aV assignment * evaluate bV assignment == evaluate cV assignment
  where
    evaluate :: GaloisField a => Either a (Poly a) -> IntMap a -> a
    evaluate (Left x) _ = x
    evaluate (Right p) w = Poly.evaluate p w
