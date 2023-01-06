module Keelung.Prof (
    compileProf,
    compileProfWithOpts
) where

import Keelung
import Keelung.Error
import Keelung.Constraint.R1CS (R1CS)

compileProf :: Encode t => FieldType -> Comp t -> IO (Either Error (R1CS Integer))
compileProf = compileProfWithOpts 1 [] []

compileProfWithOpts :: Encode t => Int -> [String] -> [String] -> FieldType -> Comp t -> IO (Either Error (R1CS Integer))
compileProfWithOpts level opts rtsopts = compileWithOpts level opts $ rtsopts <> rtsoptProf
