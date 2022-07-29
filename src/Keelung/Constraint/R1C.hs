module Keelung.Constraint.R1C where

import Data.Field.Galois (GaloisField)
import Data.IntMap (IntMap)
import Data.Semiring (times)
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
satisfyR1C :: GaloisField a => IntMap a -> R1C a -> Bool
satisfyR1C witness constraint
  | R1C aV bV cV <- constraint =
    evaluate aV witness `times` evaluate bV witness == evaluate cV witness
  where
    evaluate :: GaloisField a => Either a (Poly a) -> IntMap a -> a
    evaluate (Left x) _ = x
    evaluate (Right p) w = Poly.evaluate p w
