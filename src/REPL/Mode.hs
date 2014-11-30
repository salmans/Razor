{-|
  Razor
  Module      : REPL.Mode
  Description : This module defines what a REPL mode is
  Maintainer  : Salman Saghafi, Ryan Danas
-}
module REPL.Mode where
import Common.Model
import Common.Provenance
import Tools.Config
import Data.Maybe
import Data.List
import Syntax.GeometricUtils 
import SAT.Impl
import Chase.Impl

data REPLState = REPLState Config (Maybe Theory) (Maybe GStar) (Maybe SATIteratorType) (Maybe Model)
type GStar = (ChasePossibleFactsType, ProvInfo, SATTheoryType, Int)
type Error = String

class LoopMode mode where
	runOnce			::		mode -> REPLState -> String -> IO(Either Error REPLState)
	exitMode		::		mode -> IO()
	enterMode		::		mode -> REPLState -> IO(Either Error REPLState)
	showHelp		::		mode -> IO()
	showMode		::		mode -> IO()