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

  Module      : Common.Test.TBasic
  Description : Unit tests for Basic
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

module Common.Test.TBasic where

-- Control
import Control.Monad.State.Lazy

-- HUnit
import Test.HUnit
import Test.HUnit.Text (runTestTT)

-- Common
import Common.Basic

test_freshSymbol  = [ "test freshSymbol" ~:
                      "sym#1" ~=? evalState (freshSymbol "sym") 1 
                    , "test calling freshSymbol twice" ~:
                      "symbol#2" ~=? 
                      evalState ((freshSymbol "sym")
                                 >> (freshSymbol "symbol")) 1 ]

test_all = test_freshSymbol
