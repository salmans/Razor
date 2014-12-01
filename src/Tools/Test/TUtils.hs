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

  Module      : Tools.Test.TUtils
  Description : Unit tests for Utils
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

module Tools.Test.TUtils where

-- HUnit
import Test.HUnit
import Test.HUnit.Text (runTestTT)

-- Tools
import Tools.IUtils

test_unions = 
    [ "test empty list" ~: ([] :: [Int]) ~=? unions []
    , "test list with one element" ~: [1] ~=? unions [[1]]
    , "test list without duplicates" ~: 
      [1, 2, 3, 4] ~=? unions [[], [1,2], [3], [4]]
    , "test list with duplicates" ~: 
      [1, 2, 3, 4, 5] ~=? unions [[], [1,2], [2], [3,4], [4,5], [2]]
    ]

test_all = test_unions