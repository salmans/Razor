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

instance LoopMode TheoryM where
  runOnce	  = theoryRun
  enterMode = enterTheory
  exitMode  = exitTheory
  showHelp  = theoryHelp

data TheoryM = TheoryM

data TheoryCommand = Load TheoryFile
type TheoryFile = String

--------------------
-- Mode Functions --
--------------------
theoryRun :: TheoryM -> REPLState -> String -> IO(Either Error REPLState)
theoryRun mode state@(REPLState config theory gstar stream) command = case parseTheoryCommand command of
  Left err -> return $ Left err
  Right cmd -> case cmd of
    Load file -> putStrLn "load" >> return (Right state)
---------------------
-- chmod Functions --
---------------------
enterTheory :: TheoryM -> REPLState -> IO(Either Error REPLState)
enterTheory mode state@(REPLState config theory gstar stream) = return $ Right state

exitTheory :: TheoryM -> IO()
exitTheory mode = return ()

-----------------------
-- Command Functions --
-----------------------
theoryHelp :: TheoryM -> IO()
theoryHelp cmd = putStrLn $ 
  "ld <string>:   load the given theory by filename"++"\n"

parseTheoryCommand :: String -> Either Error TheoryCommand
parseTheoryCommand cmd = 
	let pResult = parse pCommand "parsing theory command" cmd
	in case pResult of
		Left err -> Left $ show err
		Right val -> Right $ val

pCommand :: Parser TheoryCommand
pCommand = pLoad

pLoad :: Parser TheoryCommand
pLoad = do
	symbol "ld"
	spaces
	Load <$> many (noneOf "\n")

