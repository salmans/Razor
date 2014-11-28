{-|
  Razor
  Module      : REPL.Mode
  Description : This module defines what a REPL mode is
  Maintainer  : Salman Saghafi, Ryan Danas
-}
module REPL.Mode where

type Error = String

class LoopMode state where
	runOnce			::		state -> String -> IO(state)
	exitMode		::		state -> IO()
	enterMode		::		state -> IO(Either Error state)
	showHelp		::		state -> IO()