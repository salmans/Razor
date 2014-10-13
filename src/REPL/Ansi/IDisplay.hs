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
import Data.List

fdefault = []
fhighc = [SetColor Foreground Vivid Black, SetConsoleIntensity BoldIntensity]
fhighm = [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity]
fhighy = [SetColor Foreground Vivid Yellow, SetConsoleIntensity BoldIntensity]
finfo = [SetColor Foreground Dull White, SetConsoleIntensity BoldIntensity]
ferror = [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]

displayInit :: IO()
displayInit = setSGR fdefault

displayExit :: IO()
displayExit = setSGR []

prettyPrint :: Int -> [SGR] -> String -> IO ()
prettyPrint tabs format str = do
	setSGR format
	putStr (concat $ replicate tabs "\t")
	putStr str
	setSGR []
	setSGR fdefault

prettyHighlight :: Int -> String -> String -> IO ()
prettyHighlight tabs high str = do
	let pieces = T.splitOn (T.pack high) (T.pack str)
	let highpieces = intersperse (T.pack high) pieces 
	prettyPrint tabs fdefault ""
	mapM_ (\p -> case (elemIndex p highpieces) of
		Nothing -> return ()
		Just i -> if (even i)
			then prettyPrint 0 fhighy (T.unpack p)
			else prettyPrint 0 fhighm (T.unpack p)) highpieces
