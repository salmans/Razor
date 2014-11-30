{- Razor
   Module      : REPL.Display
   Description : Internal implementation for display.
   Maintainer  : Salman Saghafi, Ryan Danas
   -}
module REPL.Display where

import Syntax.GeometricUtils 
import Common.Basic
import Control.Monad.Trans
import System.Console.Haskeline
import System.Console.ANSI
import qualified Data.Text as T
import Data.List

fuserinput = []
finput = [SetColor Foreground Dull Yellow, SetConsoleIntensity BoldIntensity]
foutput = [SetColor Foreground Dull Cyan, SetConsoleIntensity BoldIntensity]
flow = [SetColor Foreground Dull White, SetConsoleIntensity BoldIntensity]
fhigh = [SetColor Foreground Dull Black, SetColor Background Dull White, SetConsoleIntensity BoldIntensity]
ferror = [SetColor Foreground Dull Magenta, SetConsoleIntensity BoldIntensity]
ftab = "  "

displayInit :: IO()
displayInit = setSGR fuserinput

displayExit :: IO()
displayExit = setSGR []

prettyPrint :: Int -> [SGR] -> String -> IO ()
prettyPrint tabs format str = do
	setSGR format
	putStr (concat $ replicate tabs ftab)
	putStr str
	setSGR []
	setSGR fuserinput

prettyHighlight :: Int -> String -> String -> IO ()
prettyHighlight tabs high str = do
	let pieces = T.splitOn (T.pack high) (T.pack str)
	let highpieces = intersperse (T.pack high) pieces 
	prettyPrint tabs fuserinput ""
	mapM_ (\p -> case (elemIndex p highpieces) of
		Nothing -> return ()
		Just i -> if (even i)
			then prettyPrint 0 flow (T.unpack p)
			else prettyPrint 0 fhigh (T.unpack p)) highpieces

prettyTheory :: Maybe Theory -> IO()
prettyTheory thy = case thy of
	Nothing -> prettyPrint 0 ferror "No geometric theory loaded"
	Just theory -> do
		let displaytheory = intersperse "\n" $ map show theory
		mapM_ (prettyPrint 0 finput) displaytheory
