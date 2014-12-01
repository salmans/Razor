{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

class (Mode m) => LoopMode m i o | m -> i o where
	runOnce			::		m -> i -> String -> IO(Either Error o)
	update			::		m -> o -> RazorState -> (RazorState, i)
	enterMode		::		m -> RazorState -> IO(Either Error o)
	
class Mode m where
	showHelp		::		m -> IO()
	modeTag			::		m -> String
