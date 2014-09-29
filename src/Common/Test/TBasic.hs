{- Razor
   Module      : Common.Test.TBasic
   Description : Unit tests for Basic
   Maintainer  : Salman Saghafi -}

module Common.Test.TBasic where

-- Control
import Control.Monad.State.Lazy

-- HUnit
import Test.HUnit
import Test.HUnit.Text (runTestTT)

-- Common
import Common.IBasic

test_freshSymbol  = [ "test freshSymbol" ~:
                      "sym#1" ~=? evalState (freshSymbol "sym") 1 
                    , "test calling freshSymbol twice" ~:
                      "symbol#2" ~=? 
                      evalState ((freshSymbol "sym")
                                 >> (freshSymbol "symbol")) 1 ]

test_all = test_freshSymbol
