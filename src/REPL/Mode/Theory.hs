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
  
  Module      : REPL.Mode.Theory
  Description : This module defines the Theory Mode in the REPL
  Maintainer  : Salman Saghafi, Ryan Danas
-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module REPL.Mode.Theory where
import API.Surface
import REPL.Mode
import REPL.Display
import Tools.Config
import Common.Model
import qualified Data.Map as Map
import Control.Applicative hiding ((<|>), many)
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim
import Syntax.GeometricParser
import Syntax.GeometricUtils
import Syntax.IGeometric

instance LoopMode TheoryMode TheoryIn TheoryOut where
  runOnce	  = theoryRun
  update = updateTheory
  enterMode = enterTheory

instance Mode TheoryMode where
  showHelp  = theoryHelp
  modeTag   = theoryTag

data TheoryMode = TheoryM
type TheoryIn = (Config, Maybe Theory, Maybe ChaseState, ModelSpace, Maybe ModelCoordinate)
type TheoryOut = (Config, Maybe Theory, Maybe ChaseState, ModelSpace, Maybe ModelCoordinate)

data TheoryCommand = Debug | Relax | DefaultDepth Int | Load TheoryFile 
type TheoryFile = String

--------------------
-- Mode Functions --
--------------------
theoryRun :: TheoryMode -> TheoryIn -> String -> IO(Either Error TheoryOut)
theoryRun mode (config, thy, gstar, mspace, mcoor) command = case parseTheoryCommand command of
  Left err -> return $ Left err
  Right cmd -> case cmd of
    Debug -> do
      let debug' = not (configDebug config)
      prettyPrint 0 foutput $ "Debug mode is now "++(show debug')++"\n"
      return $ Right $ (config {configDebug = debug'}, thy, gstar, mspace, mcoor)
    Relax -> do
      let relax' = not (configRelaxMin config)
      prettyPrint 0 foutput $ "Relaxation of minimality constraint is now "++(show relax')++"\n"
      return $ Right $ (config {configRelaxMin = relax'}, thy, gstar, mspace, mcoor)
    DefaultDepth i -> do
      prettyPrint 0 foutput $ "Default skolem depth is now "++(show i)++"\n"
      return $ Right $ (config {configDefaultSkolemDepth = i}, thy, gstar, mspace, mcoor)
    Load file -> do
      load <- loadTheory config {configInput = Just file}
      case load of
        Right (thy', gs') -> do
          prettyTheory (Just thy')
          prettyPrint 0 foutput $ "Geometric theory loaded; ready to find models\n"
          return $ Right $ (config, Just thy', Just gs', Map.empty, Nothing)
        Left err -> return $ Left err

------------------------
-- RazorState Related --
------------------------
updateTheory :: TheoryMode -> TheoryOut -> RazorState -> (RazorState, TheoryIn)
updateTheory mode (config', theory', gstar', mspace', mcoor') state@(RazorState config theory gstar mspace mcoor) = (RazorState config' theory' gstar' mspace' mcoor', (config', theory', gstar', mspace', mcoor'))

enterTheory :: TheoryMode -> RazorState -> IO(Either Error (TheoryOut))
enterTheory mode state@(RazorState config theory gstar mspace mcoor) = return $ Right $ (config, theory, gstar, mspace, mcoor)

-----------------------
-- Command Functions --
-----------------------
theoryTag :: TheoryMode -> String
theoryTag mode = "%theory% "

theoryHelp :: TheoryMode -> IO()
theoryHelp cmd = prettyPrint 0 foutput $ ""++ 
  "<expr>:= | debug             Toggle debug mode on/off\n"++
  "         | relax             Toggle relaxation of producing only minimal models\n"++
  "         | depth <int>       Set the default skolem depth to the given integer value\n"++
  "         | load <string>     Load the given filename as an input theory\n"

parseTheoryCommand :: String -> Either Error TheoryCommand
parseTheoryCommand cmd = 
	let pResult = parse pCommand "parsing THEORYMODE command" cmd
	in case pResult of
		Left err -> Left $ show err
		Right val -> Right $ val

pCommand :: Parser TheoryCommand
pCommand = pDebug +++ pRelax +++ pDefaultDepth +++ pLoad 

pDebug :: Parser TheoryCommand
pDebug = do
  symbol "debug"
  return Debug

pRelax :: Parser TheoryCommand
pRelax = do
  symbol "relax"
  return Relax

pDefaultDepth :: Parser TheoryCommand
pDefaultDepth = do
  symbol "depth"
  spaces
  DefaultDepth <$> pInt

pInt :: Parser Int
pInt = read <$> many1 digit

pLoad :: Parser TheoryCommand
pLoad = do
  symbol "load"
  spaces
  Load <$> many (noneOf "\n\t ")
