{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Keelung.Constraint.Polynomial
  ( Poly,
    buildEither,
    buildMaybe,
    singleVar,
    vars,
    coeffs,
    mergeCoeffs,
    constant,
    view,
    mapVars,
    evaluate,
    --
    delete,
    merge,
    negate,
    substitute,
  )
where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import Data.Semiring (Semiring (..))
import Keelung.Field (N (..))
import Keelung.Types (Var)
import Prelude hiding (negate)
import qualified Prelude
import GHC.Generics (Generic)
import Data.Serialize (Serialize)

-- A Poly is a polynomial of the form "c + c₀x₀ + c₁x₁ ... cₙxₙ = 0"
--   Invariances:
--      * The coefficients are non-zone
--      * The degree of the polynomial is 1 (there's at least one variable)
data Poly n = Poly !n !(IntMap n)
  deriving (Functor, Generic)

instance Serialize n => Serialize (Poly n)

-- 2 Poly's are the same, if they have the same coefficients and variables
-- or one is the negation of the other
instance (Eq n, Num n) => Eq (Poly n) where
  (Poly c1 v1) == (Poly c2 v2) =
    if c1 == c2
      then v1 == v2 || v1 == IntMap.map Prelude.negate v2
      else (c1 == (- c2)) && (v1 == IntMap.map Prelude.negate v2)

instance (Ord n, Num n) => Ord (Poly n) where
  compare (Poly c x) (Poly d y) =
    compare (IntMap.size x, x, c) (IntMap.size y, y, d)

instance (Show n, Bounded n, Integral n, Fractional n) => Show (Poly n) where
  show (Poly n xs)
    | n == 0 = go (IntMap.toList xs)
    | otherwise = show (N n) <> " + " <> go (IntMap.toList xs)
    where
      go [] = "<empty>"
      go [term] = printTerm term
      go (term : terms) = printTerm term ++ " + " ++ go terms
      printTerm (_, 0) = error "printTerm: coefficient of 0"
      printTerm (x, 1) = "$" ++ show x
      printTerm (x, -1) = "-$" ++ show x
      printTerm (x, c) = show (N c) ++ "$" ++ show x

-- | Create a polynomial from a constant and a list of coefficients.
--   Coefficients of 0 are discarded.
buildEither :: (Eq n, Num n) => n -> [(Var, n)] -> Either n (Poly n)
buildEither c xs =
  let xs' = IntMap.filter (0 /=) $ IntMap.fromListWith (+) xs
   in if IntMap.null xs'
        then Left c
        else Right (Poly c xs')

-- | Create a polynomial from a constant and a list of coefficients.
--   Coefficients of 0 are discarded.
buildMaybe :: (Eq n, Num n) => n -> IntMap n -> Maybe (Poly n)
buildMaybe c xs =
  let xs' = IntMap.filter (0 /=) xs
   in if IntMap.null xs'
        then Nothing
        else Just (Poly c xs')

-- | Create a polynomial from a single variable and its coefficient.
singleVar :: (Eq n, Num n) => Var -> Poly n
singleVar x = Poly 0 (IntMap.singleton x 1)

-- | Return the set of variables.
vars :: Poly n -> IntSet
vars = IntMap.keysSet . coeffs

-- | Return the mapping of variables to coefficients.
coeffs :: Poly n -> IntMap n
coeffs (Poly _ xs) = xs

-- | Merge coefficients of the same variable by adding them up
mergeCoeffs :: (Eq n, Num n) => IntMap n -> IntMap n -> IntMap n
mergeCoeffs xs ys = IntMap.filter (0 /=) $ IntMap.unionWith (+) xs ys

-- | Return the constant.
constant :: Poly n -> n
constant (Poly c _) = c

-- | View pattern for Poly
view :: Poly n -> (n, IntMap n)
view (Poly c xs) = (c, xs)

-- | For renumbering the variables.
mapVars :: (Var -> Var) -> Poly n -> Poly n
mapVars f (Poly c xs) = Poly c (IntMap.mapKeys f xs)

-- | Given an assignment of variables, return the value of the polynomial.
evaluate :: Semiring n => Poly n -> IntMap n -> n
evaluate (Poly c xs) assignment =
  IntMap.foldlWithKey
    (\acc k v -> (v `times` IntMap.findWithDefault zero k assignment) `plus` acc)
    c
    xs

-- | Delete a variable from the polynomial.
delete :: (Eq n, Num n) => Var -> Poly n -> Maybe (Poly n)
delete x (Poly c xs) = buildMaybe c (IntMap.delete x xs)

-- | Merge two polynomials.
merge :: (Eq n, Num n) => Poly n -> Poly n -> Maybe (Poly n)
merge (Poly c xs) (Poly d ys) = buildMaybe (c + d) (mergeCoeffs xs ys)

-- | Negate a polynomial.
negate :: (Eq n, Num n) => Poly n -> Poly n
negate (Poly c xs) = Poly (- c) (fmap Prelude.negate xs)

-- | Substitute a variable in a polynomial with another polynomial.
substitute :: (Eq n, Num n) => Poly n -> Var -> Poly n -> Maybe (Poly n)
substitute (Poly c xs) var (Poly d ys) =
  if IntMap.member var xs
    then do
      let xs' = ys <> IntMap.delete var xs
      buildMaybe (c + d) xs'
    else return $ Poly c xs