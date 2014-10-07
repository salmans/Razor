{- Razor
   Module      : REPL.Ansi.IDisplay
   Description : Internal implementation for display.
   Maintainer  : Salman Saghafi -}

module REPL.Ansi.IDisplay where

import Common.Basic
import Control.Monad.Trans
import System.Console.Haskeline
import System.Console.ANSI
import qualified Data.Text as T

fdefault = []
fmodel = [SetColor Foreground Dull White, SetConsoleIntensity BoldIntensity]
fhigh = [SetColor Foreground Vivid Cyan, SetConsoleIntensity BoldIntensity]
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

prettyHighlight :: String -> String -> IO ()
prettyHighlight high str = do
		let pieces = T.splitOn (T.pack high) (T.pack str)
		mapM_ (\s -> do
			prettyPrint fmodel (T.unpack s)
			prettyPrint fhigh high) pieces
