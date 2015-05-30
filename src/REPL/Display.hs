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
import Common.Observation
import Control.Monad.Trans
import Control.Applicative
import System.Console.Haskeline
import System.Console.ANSI
import qualified Data.Text as T
import Data.List
import Data.Maybe

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

xmlModel :: String -> Maybe Model -> IO()
xmlModel file mdl = case mdl of
	Nothing -> prettyPrint 0 ferror "No current model!\n"
	Just (Model eqs obs) -> do
    	let (elemObs, otherObs) = partition chooseElements obs where 
    		chooseElements = (\o -> case o of
    			Obs (Rel "@Element" _) -> True
    			otherwise              -> False)
        let groupedObs = groupBy sameRelation $ sort otherObs
        let indexedObs = zip groupedObs $ map (\obs-> fromMaybe 0 (elemIndex obs groupedObs)) groupedObs
    	let str = 	"<alloy builddate=\"0\">\n"++
   			"\t<instance bitwidth=\"0\" maxseq=\"0\" command=\"\" filename=\"\">\n"++
      		"\t\t<sig label=\"univ\" ID=\"1\" builtin=\"yes\"></sig>\n"++
      		xmlDomain elemObs ++ "\n" ++
    		intercalate "\n" (xmlObservationGroup <$> indexedObs) ++ "\n" ++
    		"\t</instance>\n"++
    		"\t<source filename=\"\"/>\n"++
    		"</alloy>\n"
    	writeFile file str

xmlDomain :: [Observation] -> String
xmlDomain obs = "\t\t<sig label=\"this/Object\" ID=\"11\" parentID=\"1\" abstract=\"yes\">\n\t\t\t"++
	intercalate "\n\t\t\t" elems++
	"\n\t\t</sig>" where 
		elems = (\(Obs (Rel "@Element" [e])) -> "<atom label=\""++show e++"\"/>") <$> obs

xmlObservationGroup :: ([Observation],Int) -> String
xmlObservationGroup ([],i) = ""
xmlObservationGroup (obs,i) = case head obs of
                             (Obs (Rel sym _))   -> xmlRelationObs i sym obs
                             (Obs (FnRel sym _)) -> xmlFunctionObs i sym obs
                             (Obs (Inc _))       -> ""

xmlRelationObs :: Int -> RelSym -> [Observation] -> String
xmlRelationObs i "=" obs = ""
xmlRelationObs i sym obs = do
    let tuple  = (\(Obs (Rel _ ts)) -> xmlTuple ts) <$> obs
    let (Obs (Rel _ ts)) = head obs
    case length ts of
    	1 -> "" -- how to display unary relations in alloy? no type system
    	_ -> "\t\t<field label="++show sym++" ID=\""++show (100+i)++"\" parentID=\"11\">\n\t\t\t"++ 
    		intercalate "\n\t\t\t" tuple ++
    		"\n\t\t\t<types> " ++ (xmlTypeTuple $ length ts) ++ " </types>" ++
    		"\n\t\t</field>"

xmlFunctionObs :: Int -> FnSym -> [Observation] -> String
xmlFunctionObs i sym obss  = do
	let atoms = (\(Obs (FnRel _ ts)) -> concat (xmlAtom <$> ts)) <$> obss
	let tuple = (\(Obs (FnRel _ ts)) -> xmlTuple ts) <$> obss
	let (Obs (FnRel _ ts)) = head obss
	case length ts of
		1 -> "\t\t<sig label="++show sym++" ID=\""++show (100+i)++"\" parentID=\"11\">\n\t\t\t"++ 
			intercalate "\n\t\t\t" atoms ++
			"\n\t\t</sig>"
		_ -> "\t\t<field label="++show sym++" ID=\""++show (100+i)++"\" parentID=\"11\">\n\t\t\t"++ 
			intercalate "\n\t\t\t" tuple ++
			"\n\t\t\t<types> " ++ (xmlTypeTuple $ length ts) ++ "</types>" ++
			"\n\t\t</field>"

xmlTuple :: [Term] -> String
xmlTuple [] = ""
xmlTuple es = 	"<tuple> " ++ 
				intercalate " " (xmlAtom <$> es) ++
				" </tuple>"

xmlAtom :: Term -> String
xmlAtom (Elem e) = "<atom label=\""++show e++"\"/>"

xmlTypeTuple :: Int -> String
xmlTypeTuple 0 = ""
xmlTypeTuple n = "<type ID=\"11\"/> "++xmlTypeTuple (n-1)
