{- Razor
   Module      : Syntax.Test.TGeometric
   Description : Unit tests for Geometric formulas
   Maintainer  : Salman Saghafi -}

module Syntax.Test.TGeometric where

-- HUnit
import Test.HUnit
import Test.HUnit.Text (runTestTT)

-- Test
import Syntax.Test.TestData

-- Syntax
import Syntax.IGeometric

test_parseFormula =
    ["test parsing True" ~:
     fmla_truth ~=? (parseFormula text_truth),

     "test parsing False" ~:
     fmla_falsehood ~=? (parseFormula text_falsehood),

     "test parsing unary atoms" ~:
     fmla_Rx ~=? (parseFormula text_atomSimple1),

     "test parsing binary atoms" ~:
     fmla_Rxc ~=? (parseFormula text_atomSimple2),

     "test parsing complex atoms" ~:
     fmla_atomComplex1 ~=? (parseFormula text_atomComplex1),

     "test parsing more complex atoms" ~:
     fmla_atomComplex2 ~=? (parseFormula text_atomComplex2),

     "test parsing formula with &" ~:
     fmla_PxAndQy ~=? (parseFormula text_fmlaPxAndQy),

     "test parsing formula with |" ~:
     fmla_PxOrQy ~=? (parseFormula text_fmlaPxOrQy),

     "test parsing formula with exists without skolem function" ~:
     fmla_exists1 ~=? (parseFormula text_fmlaExists1),

     "test parsing formula with exists and skolem function" ~:
     fmla_exists2 ~=? (parseFormula text_fmlaExists2)
    ]

test_parseSequent =
    ["test trivial sequent" ~:
     seq_Triv ~=? (parseSequent text_seqTriv1),

     "test trivial sequent (empty body)" ~:
     seq_Triv ~=? (parseSequent text_seqTriv2),

     "test trivial sequent (empty head)" ~:
     seq_Triv ~=? (parseSequent text_seqTriv3),

     "test simple sequent" ~:
     seq_Simple1 ~=? (parseSequent text_seqSimple1),

     "test bodiless sequent" ~:
     seq_Simple2 ~=? (parseSequent text_seqSimple2),

     "test headless sequent" ~:
     seq_Simple3 ~=? (parseSequent text_seqSimple3),

     "test complex sequent" ~:
     seq_Complex1 ~=? (parseSequent text_seqComplex1),

     "test headless sequent" ~:
     seq_Complex2 ~=? (parseSequent text_seqComplex2),

     "test headless sequent" ~:
     seq_Complex3 ~=? (parseSequent text_seqComplex3)
    ]

test_all = test_parseFormula ++ test_parseSequent