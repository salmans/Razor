{- Razor
   Module      : Tools.Test.TCounter
   Description : Unit tests for Counter
   Maintainer  : Salman Saghafi -}

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