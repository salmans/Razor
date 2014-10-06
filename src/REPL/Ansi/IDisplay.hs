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
finfo = [SetColor Foreground Vivid Blue, SetConsoleIntensity BoldIntensity]
fwarning = [SetColor Foreground Vivid Yellow, SetConsoleIntensity BoldIntensity]
ferror = [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]

displayInit :: IO()
displayInit = setSGR fdefault

displayExit :: IO()
displayExit = setSGR []

prettyPrint :: String -> [SGR] -> IO ()
prettyPrint str format = do
	setSGR format
	putStr str
	setSGR []
	setSGR fdefault

prettyREPL :: String -> [SGR] -> InputT IO ()
prettyREPL str format = lift $ do
	setSGR format
	putStr str
	setSGR []
	setSGR fdefault