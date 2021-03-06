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
import API.Core 
import qualified Data.Map as Map 

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
      highpieces = intersperse (T.pack high) pieces 
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
  Just model@(Model eqs obs) -> do
        -- rework the model into workable data structures
        let (elemObs, otherObs) = partition chooseElements obs where 
                chooseElements (Obs (Rel "@Element" _)) = True
                chooseElements _                        = False
            groupedObs = groupBy sameRelation $ sort otherObs
            indexedObs = zip groupedObs $ map (\obs-> fromMaybe 0 (elemIndex obs groupedObs)) groupedObs
            (unaryObs, multiObs) = partition unary obs where
                unary (Obs (Rel _ [e])) = True
                unary (Obs (FnRel _ [e])) = True
                unary _ = False
            typeMap = foldr (addTypeMap) Map.empty unaryObs
            typeList = Map.toList typeMap
            indexedTypes = zip typeList $ map (\obs-> fromMaybe 0 (elemIndex obs typeList)) typeList
        -- construct xml file from data structures
            str = 
                "<alloy builddate=\"0\">\n"++
                "\t<instance bitwidth=\"0\" maxseq=\"0\" command=\"\" filename=\"\">\n"++
                "\t\t<sig label=\"univ\" ID=\"1\" builtin=\"yes\"></sig>\n"++
                xmlDomain elemObs ++ "\n" ++
                intercalate "\n" (xmlObservationGroup <$> indexedObs) ++ "\n" ++
                intercalate "\n" (xmlTypeGroup <$> indexedTypes) ++ "\n" ++
                "\t</instance>\n"++
                "\t<source filename=\"\"/>\n"++
                "</alloy>\n"
        writeFile file $ str

addTypeMap :: Observation -> Map.Map Element [Sym] -> Map.Map Element [Sym]
addTypeMap (Obs (Rel sym [(Elem e)])) m = Map.insert e (sym:(fromMaybe [] (Map.lookup e m))) m
addTypeMap (Obs (FnRel sym [(Elem e)])) m = Map.insert e (sym:(fromMaybe [] (Map.lookup e m))) m

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
            (Obs (Rel _ ts)) = head obs
        case length ts of
          1 -> "" --dealt with adhoc
          _ -> "\t\t<field label="++show sym++" ID=\""++show (100+i)++"\" parentID=\"11\">\n\t\t\t"++ 
                intercalate "\n\t\t\t" tuple ++
                "\n\t\t\t<types> " ++ (xmlTypeTuple $ length ts) ++ " </types>" ++
                "\n\t\t</field>"

xmlFunctionObs :: Int -> FnSym -> [Observation] -> String
xmlFunctionObs i sym obss  = do
  let atoms = (\(Obs (FnRel _ ts)) -> concat (xmlAtom <$> ts)) <$> obss
      tuple = (\(Obs (FnRel _ ts)) -> xmlTuple ts) <$> obss
      (Obs (FnRel _ ts)) = head obss
  case length ts of
        1 -> "" -- dealt with adhoc
        _ -> "\t\t<field label="++show sym++" ID=\""++show (100+i)++"\" parentID=\"11\">\n\t\t\t"++ 
          intercalate "\n\t\t\t" tuple ++
          "\n\t\t\t<types> " ++ (xmlTypeTuple $ length ts) ++ "</types>" ++
          "\n\t\t</field>"

xmlTuple :: [Term] -> String
xmlTuple [] = ""
xmlTuple es =   "<tuple> " ++ 
                intercalate " " (xmlAtom <$> es) ++
                " </tuple>"

xmlAtom :: Term -> String
xmlAtom (Elem e) = "<atom label=\""++show e++"\"/>"

xmlTypeTuple :: Int -> String
xmlTypeTuple 0 = ""
xmlTypeTuple n = "<type ID=\"11\"/> "++xmlTypeTuple (n-1)

xmlTypeGroup :: ((Element,[Sym]), Int) -> String
xmlTypeGroup ((e,syms),i) = xmlTypes i e (typesOf e syms []) where
  typesOf e [] types = types
  typesOf e ["@Element"] [] = [show e]
  typesOf e ("@Element":rest) types = typesOf e rest types
  typesOf e (sym:rest) types = typesOf e rest (sym:types)

xmlTypes :: Int -> Element -> [Sym] -> String
xmlTypes i e syms = "\t\t<sig label=\""++typestring++"\" ID=\""++show (200+i)++"\" parentID=\"11\">\n\t\t\t"++ 
          intercalate "\n\t\t\t" [xmlAtom (Elem e)] ++
          "\n\t\t</sig>" where
                typestring = intercalate "/" syms

