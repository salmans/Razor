{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
  Razor
  Module      : REPL.Mode.Theory
  Description : This module defines the Theory Mode in the REPL
  Maintainer  : Salman Saghafi, Ryan Danas
-}
module REPL.Mode.Theory where
import Control.Applicative hiding ((<|>), many)
import REPL.Mode
import REPL.Display
import Tools.Config
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim
import Syntax.GeometricParser

instance LoopMode TheoryState where
  runOnce	  = theoryRun
  enterMode = enterTheory
  exitMode  = exitTheory
  showHelp  = theoryHelp

type TheoryState = Config

data TheoryCommand = Load TheoryFile | ShowTheory
type TheoryFile = String

--------------------
-- Mode Functions --
--------------------
theoryRun :: TheoryState -> String -> IO(TheoryState)
theoryRun state command = case parseTheoryCommand command of
  Left err -> putStrLn "error" >> return state
  Right cmd -> case cmd of
    Load file -> putStrLn "load" >> return state
    ShowTheory -> putStrLn "theory" >> return state

---------------------
-- chmod Functions --
---------------------
enterTheory :: TheoryState -> IO(Either Error TheoryState)
enterTheory state = return $ Right state

exitTheory :: TheoryState -> IO()
exitTheory state = return ()

-----------------------
-- Command Functions --
-----------------------
parseTheoryCommand :: String -> Either Error TheoryCommand
parseTheoryCommand cmd = 
	let pResult = parse pCommand "parsing theory command" cmd
	in case pResult of
		Left err -> Left $ show err
		Right val -> Right $ val

pCommand :: Parser TheoryCommand
pCommand = pLoad <|> pShow

pLoad :: Parser TheoryCommand
pLoad = do
	symbol "ld"
	spaces
	Load <$> many (noneOf "\n")

pShow :: Parser TheoryCommand
pShow = do
  symbol "!"
  spaces
  return ShowTheory

theoryHelp :: TheoryState -> IO()
theoryHelp cmd = putStrLn $ 
  "!:             show the input theory"++"\n"++
  "ld <string>:   load the given theory by filename"++"\n"
