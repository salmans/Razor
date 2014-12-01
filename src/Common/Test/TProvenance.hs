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

  Module      : Common.Test.TProvenance
  Description : Unit tests for Provenance
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

module Common.Test.TProvenance where

-- Standard
import Data.Bimap as Bimap

-- HUnit
import Test.HUnit
import Test.HUnit.Text (runTestTT)

-- Test
import Syntax.Test.TestData
import Common.Test.TestData

-- Common
import Common.IProvenance

test_emptyProvInfo = 
    [ "test emptyProvInfo" ~:
      ProvInfo Bimap.empty ~=? emptyProvInfo
    ]

test_emptyProvInfoWithConstants =
    [ "test emptyProvInfoWithConstants for empty set of constants" ~:
      emptyProvInfo ~=? emptyProvInfoWithConstants []
    , "test emptyProvInfoWithConstants for empty set of constants" ~:
      prov_prov1 ~=? emptyProvInfoWithConstants [cons "c1", cons "c2"]
    ]

test_getElementProv =
    [ "test getElementProv for existing provenance" ~:
      Just (consTerm "c1") ~=? getElementProv (elm "c1") elmprov_prov1
    , "test getElementProv for non-existing provenance" ~:
      Nothing   ~=? getElementProv (elm "c3") elmprov_prov1
    ]

test_addElementProv =
    [ "test addElementProv for constant provenance" ~:
      elmprov_prov2 ~=? addElementProv (elm "e") "a" [] elmprov_prov1
    , "test addElementProv for provenance with skolem function" ~:
      elmprov_prov3 ~=? 
      addElementProv (elm "e") "f" [elm "c1", elm "c2"] elmprov_prov1
    -- , "test addElementProv for provenance with skolem function" ~:
    --   elmprov_prov3 ~=? 
    --   addElementProv (elm "e") "f" [elm "c1", elm "c3"] elmprov_prov1
    -- this should throw an exception
    ]

test_modifyElementProvs = 
    [ "test modifyElementProvs" ~:
      prov_prov2 ~=? 
      modifyElementProvs (addElementProv (elm "e") "a" []) prov_prov1
    ]

test_all = test_emptyProvInfo ++ test_emptyProvInfoWithConstants
           ++ test_getElementProv ++ test_addElementProv
           ++ test_modifyElementProvs
