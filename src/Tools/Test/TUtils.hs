{- Razor
   Module      : Tools.Test.TUtils
   Description : Unit tests for Utils
   Maintainer  : Salman Saghafi -}

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