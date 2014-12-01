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

  Module      : AllUnitTests
  Description : Runs all unit tests.
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

module Test.AllUnitTests where


import Test.HUnit
import Test.HUnit.Text (runTestTT)

-- Common
import qualified Common.Test.TBasic as TBasic
import qualified Common.Test.TObservation as TObservation
import qualified Common.Test.TProvenance as TProvenance

-- Syntax
import qualified Syntax.Test.TTerm as TTerm
import qualified Syntax.Test.TGeometric as TGeometric
import qualified Syntax.Test.TFirstOrder as TFirstOrder
import qualified Syntax.Test.TGeometricUtils as TGeometricUtils
import qualified Syntax.Test.TFirstOrderUtils as TFirstOrderUtils

-- RelAlg
import qualified Chase.PossibleFacts.RelAlg.Test.TDB as TDB
import qualified Chase.PossibleFacts.RelAlg.Test.TLang as TLang
import qualified Chase.PossibleFacts.RelAlg.Test.TTranslate as TTranslate

-- Chase
import qualified Chase.Test.TChase as TChase

-- Tools
import qualified Tools.Test.TUtils as TUtils
import qualified Tools.Test.TCounter as TCounter


runAllTests = runTestTT $ TestList $ 
              -- Common
              TBasic.test_all
              ++ TObservation.test_all
              ++ TProvenance.test_all

              -- Tools
              ++ TUtils.test_all ++ TCounter.test_all 

              -- Syntax
              ++ TTerm.test_all 
              ++ TGeometric.test_all ++ TFirstOrder.test_all
              ++ TGeometricUtils.test_all ++ TFirstOrderUtils.test_all

              -- Chase.PossibleFacts.RelAlg
              ++ TTranslate.test_all ++ TLang.test_all ++ TDB.test_all

              -- Chase
              ++ TChase.test_all

