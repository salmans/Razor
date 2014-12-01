{- This file is part of Razor.

  Razor is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Razor is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Razor.  If not, see <http://www.gnu.org/licenses/>.

  Module      : REPL.Display
  Description : Displays various data-structures.
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}
module REPL.Display where

import Syntax.GeometricUtils 
import Common.Basic
import Common.Model
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
	Nothing -> prettyPrint 0 ferror "No geometric theory loaded!\n"
	Just theory -> do
		let displaytheory = intersperse "\n" $ map show theory
		mapM_ (\s->prettyPrint 0 finput ((show s)++"\n")) theory

prettyModel :: Maybe Model -> IO()
prettyModel mdl = case mdl of
	Nothing -> prettyPrint 0 ferror "No current model!\n"
	Just model -> prettyPrint 0 flow (show model)
