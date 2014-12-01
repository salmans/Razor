{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
  Razor
  Module      : REPL.Mode.Query
  Description : This module defines the query mode in the REPL.
  Maintainer  : Salman Saghafi, Ryan Danas
-}
module REPL.Mode.Query where
import API.Surface
import Common.Model
import Common.Provenance
import SAT.Impl
import REPL.Mode
import REPL.Display
import Tools.Config
import Control.Applicative hiding ((<|>), many)
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim
import Syntax.GeometricParser
import Syntax.GeometricUtils
import Syntax.ITerm
import Data.List
import Syntax.IGeometric


instance LoopMode QueryMode QueryIn QueryOut where
  runOnce	  = queryRun
  update    = updateQuery
  enterMode = enterQuery

instance Mode QueryMode where
  showHelp  = queryHelp
  modeTag   = queryTag

data QueryMode = QueryM
type QueryIn = (Theory, GStar, Model)
type QueryOut = (Theory, GStar, Model)

data QueryCommand = Blame Formula | Name Bool Bool Term

--------------------
-- Mode Functions --
--------------------
queryRun :: QueryMode -> QueryIn -> String -> IO(Either Error QueryOut)
queryRun mode state@(theory, gstar, model) command = case parseQueryCommand command of
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
updateQuery :: QueryMode -> QueryOut -> RazorState -> (RazorState, QueryIn)
updateQuery mode (theory, gstar, model) state = (state, (theory, gstar, model))

enterQuery :: QueryMode -> RazorState -> IO(Either Error QueryOut)
enterQuery mode state@(RazorState config theory gstar stream model) = case (theory, gstar, model) of
  (Just theory', Just gstar', Just model') -> return $ Right (theory', gstar', model')
  _ -> return $ Left "Current model not initialized by another mode!"

-----------------------
-- Command Functions --
-----------------------
queryTag :: QueryMode -> String
queryTag mode = "%query% "

queryHelp :: QueryMode -> IO()
queryHelp cmd = prettyPrint 0 foutput $ ""++
  "<expr>:= | origin<all_flag><rec_flag> <element>    Display the rule that caused this element to exist\n"++
  "  <all_flag>:=   |                                   Only display the origin of the given element\n"++
  "                 | s                                 Also display the equivalence class origins\n"++
  "  <rec_flag>:=   |                                   Only display the origin of the given element\n"++
  "                 | *                                 Recursively display all origins that caused the rule to fire\n"++
  "         | blame <formula>                         Display the rule that fired to make the given fact true\n"

parseQueryCommand :: String -> Either Error QueryCommand
parseQueryCommand cmd = 
  let pResult = parse pCommand "parsing QUERY command" cmd
  in case pResult of
    Left err -> Left $ show err
    Right val -> Right $ val

pCommand :: Parser QueryCommand
pCommand = pName <|> pBlame

pName :: Parser QueryCommand
pName = pNameOne +++ pNameAll
pNameOne :: Parser QueryCommand
pNameOne = do
  symbol "origin"
  pNameOneHead +++ pNameOneRec
pNameOneHead :: Parser QueryCommand
pNameOneHead = do
  symbol ""
  Name False False <$> pElement
pNameOneRec :: Parser QueryCommand
pNameOneRec = do
  symbol "*"
  Name False True <$> pElement
pNameAll :: Parser QueryCommand
pNameAll = do
  symbol "origins"
  pNameAllHead +++ pNameAllRec
pNameAllHead :: Parser QueryCommand
pNameAllHead = do
  symbol ""
  Name True False <$> pElement
pNameAllRec :: Parser QueryCommand
pNameAllRec = do
  symbol "*"
  Name True True <$> pElement

pBlame :: Parser QueryCommand
pBlame = do
  symbol "blame"
  Blame <$> xpFactor
