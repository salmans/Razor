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
import Control.Monad (liftM)
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim
import Text.Parsec.Token
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
type TheoryIn = (Config, Maybe Theory, ModelSpace, Maybe ModelCoordinate)
type TheoryOut = (Config, Maybe Theory, ModelSpace, Maybe ModelCoordinate)

data TheoryCommand = Debug | Relax | DefaultDepth Int | Load TheoryFile Bool
type TheoryFile = String

--------------------
-- Mode Functions --
--------------------
theoryRun :: TheoryMode -> TheoryIn -> String -> IO(Either Error TheoryOut)
theoryRun mode (config, thy, mspace, mcoor) command = case parseTheoryCommand command of
  Left err -> return $ Left err
  Right cmd -> case cmd of
    Debug -> do
      let debug' = not (configDebug config)
      prettyPrint 0 foutput $ "Debug mode is now "++(show debug')++"\n"
      return $ Right $ (config {configDebug = debug'}, thy, mspace, mcoor)
    Relax -> do
      let relax' = not (configRelaxMin config)
      prettyPrint 0 foutput $ "Relaxation of minimality constraint is now "++(show relax')++"\n"
      return $ Right $ (config {configRelaxMin = relax'}, thy, mspace, mcoor)
    DefaultDepth i -> do
      prettyPrint 0 foutput $ "Default skolem depth is now "++(show i)++"\n"
      return $ Right $ (config {configDefaultSkolemDepth = i}, thy, mspace, mcoor)
    Load file isTPTP -> do
      prettyPrint 0 foutput $ "Loading Theory...\n"
      load <- loadTheory (config {configInput = Just file}) isTPTP
      case load of
        Right (cfg', thy') -> do
          prettyTheory (Just thy')
          prettyPrint 0 foutput $ "Geometric theory loaded; ready to find models\n"
          return $ Right $ (cfg', Just thy', Map.empty, Nothing)
        Left err -> return $ Left err

------------------------
-- RazorState Related --
------------------------
updateTheory :: TheoryMode -> TheoryOut -> RazorState -> (RazorState, TheoryIn)
updateTheory mode (config', theory', mspace', mcoor') state@(RazorState config theory mspace mcoor) = (RazorState config' theory' mspace' mcoor', (config', theory', mspace', mcoor'))

enterTheory :: TheoryMode -> RazorState -> IO(Either Error (TheoryOut))
enterTheory mode state@(RazorState config theory mspace mcoor) = return $ Right $ (config, theory, mspace, mcoor)

-----------------------
-- Command Functions --
-----------------------
theoryTag :: TheoryMode -> String
theoryTag mode = "theory"

theoryHelp :: TheoryMode -> IO()
theoryHelp cmd = prettyPrint 0 foutput $ ""++ 
  "<expr>:= | debug             Toggle debug mode on/off\n"++
  "         | relax             Toggle relaxation of producing only minimal models\n"++
  "         | depth <int>       Set the default skolem depth to the given integer value\n"++
  "         | load <string>     Load the given filename as a Razor input theory\n"++
  "         | tptp <string>     (Beta) Load the given filename as a TPTP input theory\n"

parseTheoryCommand :: String -> Either Error TheoryCommand
parseTheoryCommand cmd = 
	let pResult = parse pCommand "parsing THEORYMODE command" cmd
	in case pResult of
		Left err -> Left $ show err
		Right val -> Right $ val

pCommand :: Parser TheoryCommand
pCommand = pDebug +++ pRelax +++ pDefaultDepth +++ pLoad +++ pTPTP 

pDebug :: Parser TheoryCommand
pDebug = do
  string "debug"
  return Debug

pRelax :: Parser TheoryCommand
pRelax = do
  string "relax"
  return Relax

pDefaultDepth :: Parser TheoryCommand
pDefaultDepth = do
  string "depth"
  spaces
  DefaultDepth <$> (pInt <|> pNone)

pInt :: Parser Int
pInt = read <$> many1 digit

pNone :: Parser Int
pNone = do
  string "-1"
  return (-1)

pLoad :: Parser TheoryCommand
pLoad = do
  string "load"
  spaces
  filename <- many (noneOf "\n\t ")
  return $ Load filename False

pTPTP :: Parser TheoryCommand
pTPTP = do
  string "tptp"
  spaces
  filename <- many (noneOf "\n\t ")
  return $ Load filename True
