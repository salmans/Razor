{- Razor
   Module      : REPL.Ansi.IDisplay
   Description : Internal implementation for display.
   Maintainer  : Salman Saghafi -}

module REPL.Ansi.IDisplay where

import Control.Monad.Trans
import System.Console.Haskeline
import System.Console.ANSI

fdefault = []
fmodel = [SetColor Foreground Dull White, SetConsoleIntensity BoldIntensity]
finfo = [SetColor Foreground Dull Blue, SetConsoleIntensity BoldIntensity]
fwarning = [SetColor Foreground Dull Yellow, SetConsoleIntensity BoldIntensity]
ferror = [SetColor Foreground Dull Red, SetConsoleIntensity BoldIntensity]

displayInit :: IO()
displayInit = setSGR fdefault

displayExit :: IO()
displayExit = setSGR []

prettyPrint :: [SGR] -> String -> IO ()
prettyPrint format str = do
	setSGR format
	putStr str
	setSGR []
	setSGR fdefault

prettyREPL :: [SGR] -> String -> InputT IO ()
prettyREPL format str = lift $ do
	setSGR format
	putStr str
	setSGR []
	setSGR fdefault