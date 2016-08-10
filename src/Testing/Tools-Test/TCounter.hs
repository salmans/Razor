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

  Module      : Tools.Test.TCounter
  Description : Unit tests for Counter
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

module Tools.Test.TCounter where

-- HUnit
import Test.HUnit
import Test.HUnit.Text (runTestTT)

-- Control
import Control.Monad.State.Lazy

-- Tools
import Tools.ICounter

test_increment = 
    [ "test increment" ~: 0 ~=? evalState increment 0
    , "test increment state" ~: 1 ~=? execState increment 0
    , "test increment twice" ~: 
      1 ~=? evalState (increment >> increment) 0]

test_all = test_increment