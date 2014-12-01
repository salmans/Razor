{- This file is part of Razor.

  Razor is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Razor is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Razor.  If not, see <http://www.gnu.org/licenses/>.
  
  Module      : Syntax.Test.TFirstOrder
  Description : Unit tests for first-order formulas
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

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