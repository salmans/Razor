{- Razor
   Module      : REPL.Ansi.IDisplay
   Description : Internal implementation for display.
   Maintainer  : Salman Saghafi -}

module REPL.Ansi.IDisplay where

import Control.Monad.Trans
import System.Console.Haskeline
import System.Console.ANSI

fmodel = [SetColor Foreground Dull Green, SetConsoleIntensity BoldIntensity]
finfo = [SetColor Foreground Dull Blue, SetConsoleIntensity BoldIntensity]
fwarning = [SetColor Foreground Dull Yellow, SetConsoleIntensity BoldIntensity]
ferror = [SetColor Foreground Dull Red, SetConsoleIntensity BoldIntensity]

prettyPrint :: String -> [SGR] -> IO ()
prettyPrint str format = do
	setSGR format
	putStr str
	setSGR []

prettyREPL :: String -> [SGR] -> InputT IO ()
prettyREPL str format = lift $ do
	setSGR format
	putStr str
	setSGR []