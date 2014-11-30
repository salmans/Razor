{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
  Razor
  Module      : REPL.Mode.Theory
  Description : This module defines the Theory Mode in the REPL
  Maintainer  : Salman Saghafi, Ryan Danas
-}
module REPL.Mode.Theory where
import API.Surface
import REPL.Mode
import REPL.Display
import Tools.Config
import Control.Applicative hiding ((<|>), many)
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim
import Syntax.GeometricParser

instance LoopMode TheoryM where
  runOnce	  = theoryRun
  enterMode = enterTheory
  exitMode  = exitTheory
  showHelp  = theoryHelp
  modeTag   = theoryTag

data TheoryM = TheoryM

data TheoryCommand = Load TheoryFile
type TheoryFile = String

--------------------
-- Mode Functions --
--------------------
theoryRun :: TheoryM -> REPLState -> String -> IO(Either Error REPLState)
theoryRun mode state@(REPLState config theory gstar stream model) command = case parseTheoryCommand command of
  Left err -> return $ Left err
  Right cmd -> case cmd of
    Load file -> do
      load <- loadTheory config file
      case load of
        Right (thy', gs') -> do
          prettyTheory (Just thy')
          return $ Right (REPLState config (Just thy') (Just gs') stream model)
        Left err -> return $ Left err

---------------------
-- chmod Functions --
---------------------
enterTheory :: TheoryM -> REPLState -> IO(Either Error REPLState)
enterTheory mode state@(REPLState config theory gstar stream model) = return $ Right state

exitTheory :: TheoryM -> IO()
exitTheory mode = return ()

-----------------------
-- Command Functions --
-----------------------
theoryTag :: TheoryM -> String
theoryTag mode = "%theory% "

theoryHelp :: TheoryM -> IO()
theoryHelp cmd = prettyPrint 0 foutput $ ""++ 
  "ld <string>:   load the given theory by filename"

parseTheoryCommand :: String -> Either Error TheoryCommand
parseTheoryCommand cmd = 
	let pResult = parse pCommand "parsing THEORYMODE command" cmd
	in case pResult of
		Left err -> Left $ show err
		Right val -> Right $ val

pCommand :: Parser TheoryCommand
pCommand = pLoad

pLoad :: Parser TheoryCommand
pLoad = do
	symbol "ld"
	spaces
	Load <$> many (noneOf "\n\t ")

