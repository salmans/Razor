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

  Module      : REPL.Mode.Explain
  Description : This module defines the query mode in the REPL.
  Maintainer  : Salman Saghafi, Ryan Danas
-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module REPL.Mode.Explain where
import API.Surface
import Common.Model
import qualified Data.Map as Map
import Common.Provenance
import SAT.Impl
import REPL.Mode
import REPL.Display
import Tools.Config
import Control.Applicative hiding ((<|>), many)
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim
import Text.Parsec.Token
import Syntax.GeometricParser
import Syntax.ITerm
import Data.List
import Syntax.IGeometric


instance LoopMode ExplainMode ExplainIn ExplainOut where
  runOnce	  = queryRun
  update    = updateExplain
  enterMode = enterExplain

instance Mode ExplainMode where
  showHelp  = queryHelp
  modeTag   = queryTag

data ExplainMode = ExplainM
type ExplainIn = (Theory, ChaseState, Model)
type ExplainOut = (Theory, ChaseState, Model)

data ExplainCommand = Blame Formula | Name Bool Bool Term

--------------------
-- Mode Functions --
--------------------
queryRun :: ExplainMode -> ExplainIn -> String -> IO(Either Error ExplainOut)
queryRun mode state@(theory, gstar, model) command = case parseExplainCommand command of
  Left err -> return $ Left err
  Right cmd -> case cmd of
    Blame fml -> do
      let justification = getJustification gstar model fml
      prettyPrint 0 foutput $ "justification of "++(show fml)++"\n"
      printBlame theory justification 0 (show fml)
      return $ Right state
    Name isall isrec term -> do
      let origins = getOrigin theory gstar model isrec term
      if isall
        then mapM_ (printOrigin theory 0) origins
        else printOrigin theory 0 (head origins)
      return $ Right state

-------------
-- Helpers --
-------------
printDiff :: Sequent -> Sequent -> (String, Int) -> IO()
printDiff original diff format@(highlight, tabs) = do
  prettyPrint tabs finput ("thy rule: "++(show original)++"\n")
  prettyHighlight tabs highlight ("instance: "++(show diff)++"\n")

printBlame :: Theory -> QBlame -> Int -> String -> IO()
printBlame thy blame tabs highlight = case blame of
  Left err -> prettyPrint tabs ferror (err++"\n")
  Right ((TheoryBlame i sub), blamed) -> printDiff (thy !! (i-1)) blamed (highlight,tabs)
  Right ((UserBlame augmentation), blamed) -> prettyPrint tabs finput ("user augmentation: "++(show augmentation)++"\n")

printOrigin :: Theory -> Int -> QOrigin -> IO ()
printOrigin thy tabs (QOriginLeaf term origin) = do
  prettyPrint tabs foutput ("origin of "++(show term)++"\n")
  printBlame thy origin tabs (show term)
printOrigin thy tabs (QOriginNode term origin depends) = do
  printOrigin thy tabs (QOriginLeaf term origin)
  mapM_ (printOrigin thy (tabs+1)) depends

------------------------
-- RazorState Related --
------------------------
updateExplain :: ExplainMode -> ExplainOut -> RazorState -> (RazorState, ExplainIn)
updateExplain mode (theory, gstar, model) state = (state, (theory, gstar, model))

enterExplain :: ExplainMode -> RazorState -> IO(Either Error ExplainOut)
enterExplain mode state@(RazorState config theory mspace mcoor) = case (theory, mcoor) of
  (Just theory', Just mcoor') -> case Map.lookup mcoor' mspace of
    Nothing -> return $ Left "Current model not initialized by another mode!"
    Just (chasestate, model') -> do
      prettyModel $ Just model'
      prettyPrint 0 foutput "Running queries over this model\n"
      return $ Right (theory', chasestate, model')
  _ -> return $ Left "Current model not initialized by another mode!"

-----------------------
-- Command Functions --
-----------------------
queryTag :: ExplainMode -> String
queryTag mode = "%explain% "

queryHelp :: ExplainMode -> IO()
queryHelp cmd = prettyPrint 0 foutput $ ""++
  "<expr>:= | origin<all_flag><rec_flag> <element>    Display the rule that caused this element to exist\n"++
  "  <all_flag>:=   |                                   Only display the origin of the given element\n"++
  "                 | s                                 Also display the origins of the representatives in this element's equivalence class\n"++
  "  <rec_flag>:=   |                                   Only display the origin of the given element\n"++
  "                 | *                                 Recursively display all origins that caused the rule to fire\n"++
  "         | blame <formula>                         Display the rule that fired to make the given fact true\n"

parseExplainCommand :: String -> Either Error ExplainCommand
parseExplainCommand cmd = 
  let pResult = parse pCommand "parsing EXPLAIN command" cmd
  in case pResult of
    Left err -> Left $ show err
    Right val -> Right $ val

pCommand :: Parser ExplainCommand
pCommand = pName <|> pBlame

pName :: Parser ExplainCommand
pName = pNameOne +++ pNameAll
pNameOne :: Parser ExplainCommand
pNameOne = do
  string "origin"
  pNameOneHead +++ pNameOneRec
pNameOneHead :: Parser ExplainCommand
pNameOneHead = do
  string ""
  Name False False <$> pElement
pNameOneRec :: Parser ExplainCommand
pNameOneRec = do
  string "*"
  Name False True <$> pElement
pNameAll :: Parser ExplainCommand
pNameAll = do
  string "origins"
  pNameAllHead +++ pNameAllRec
pNameAllHead :: Parser ExplainCommand
pNameAllHead = do
  string ""
  Name True False <$> pElement
pNameAllRec :: Parser ExplainCommand
pNameAllRec = do
  string "*"
  Name True True <$> pElement

pBlame :: Parser ExplainCommand
pBlame = do
  string "blame"
  Blame <$> xpFactor
