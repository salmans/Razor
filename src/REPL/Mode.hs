{-|
  Razor
  Module      : REPL.Mode
  Description : This module defines what a REPL mode is
  Maintainer  : Salman Saghafi, Ryan Danas
-}
module REPL.Mode where
import API.Surface
import Common.Model
import Common.Provenance
import Tools.Config
import Data.Maybe
import Data.List
import Syntax.GeometricUtils 
import SAT.Impl
import Chase.Impl

class LoopMode mode where
	runOnce			::		mode -> RazorState -> String -> IO(Either Error RazorState)
	enterMode		::		mode -> RazorState -> IO(Either Error (RazorState, mode))
	showHelp		::		mode -> IO()
	modeTag			::		mode -> String
