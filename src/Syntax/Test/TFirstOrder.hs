{- Razor
   Module      : Syntax.Test.TFirstOrder
   Description : Unit tests for first-order formulas
   Maintainer  : Salman Saghafi -}

module Syntax.Test.TFirstOrder where

-- HUnit
import Test.HUnit
import Test.HUnit.Text (runTestTT)

-- Test
import Syntax.Test.TestData

-- Syntax
import Syntax.IFirstOrder

test_parseFormula =
    ["test parsing fof True" ~:
     fof_truth ~=? (parseFormula text_truth),

     "test parsing fof False" ~:
     fof_falsehood ~=? (parseFormula text_falsehood),

     "test parsing fof unary atoms" ~:
     fof_Rx ~=? (parseFormula text_atomSimple1),

     "test parsing fof binary atoms" ~:
     fof_Rxc ~=? (parseFormula text_atomSimple2),

     "test parsing fof complex atoms" ~:
     fof_atomComplex1 ~=? (parseFormula text_atomComplex1),

     "test parsing fof more complex atoms" ~:
     fof_atomComplex2 ~=? (parseFormula text_atomComplex2),

     "test parsing fof formula with &" ~:
     fof_PxAndQy ~=? (parseFormula text_fmlaPxAndQy),

     "test parsing fof formula with |" ~:
     fof_PxOrQy ~=? (parseFormula text_fmlaPxOrQy),

     "test parsing fof formula with exists without skolem function" ~:
     fof_exists1 ~=? (parseFormula text_fmlaExists1),

     "test parsing fof formula with exists with skolem function" ~:
     fof_exists2 ~=? (parseFormula text_fmlaExists2)
    ]

test_all = test_parseFormula