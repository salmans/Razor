{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Syntax.GeometricUtils

instance LoopMode TheoryMode TheoryIn TheoryOut where
  runOnce	  = theoryRun
  update = updateTheory
  enterMode = enterTheory

instance Mode TheoryMode where
  showHelp  = theoryHelp
  modeTag   = theoryTag

data TheoryMode = TheoryM
type TheoryIn = Config
type TheoryOut = (Maybe Theory, Maybe GStar)

data TheoryCommand = Load TheoryFile
type TheoryFile = String

--------------------
-- Mode Functions --
--------------------
theoryRun :: TheoryMode -> TheoryIn -> String -> IO(Either Error TheoryOut)
theoryRun mode config command = case parseTheoryCommand command of
  Left err -> return $ Left err
  Right cmd -> case cmd of
    Load file -> do
      load <- loadTheory config file
      case load of
        Right (thy', gs') -> do
          prettyTheory (Just thy')
          return $ Right $ (Just thy', Just gs')
        Left err -> return $ Left err

------------------------
-- RazorState Related --
------------------------
updateTheory :: TheoryMode -> TheoryOut -> RazorState -> (RazorState, TheoryIn)
updateTheory mode (theory', gstar') state@(RazorState config theory gstar stream model) = (RazorState config theory' gstar' stream model, config)

enterTheory :: TheoryMode -> RazorState -> IO(Either Error (TheoryOut))
enterTheory mode state@(RazorState config theory gstar stream model) = return $ Right $ (theory, gstar)

-----------------------
-- Command Functions --
-----------------------
theoryTag :: TheoryMode -> String
theoryTag mode = "%theory% "

theoryHelp :: TheoryMode -> IO()
theoryHelp cmd = prettyPrint 0 foutput $ ""++ 
  "<expr>:= |   ld <string>       Load the given filename as an input theory"

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

