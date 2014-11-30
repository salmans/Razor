{-|
  Razor
  Module      : REPL.Mode.Stream
  Description : This module defines the Stream (Horizontal Model Exploration) Mode in the REPL
  Maintainer  : Salman Saghafi, Ryan Danas
-}
module REPL.Mode.Stream where
import API.Surface
import REPL.Mode
import REPL.Display
import Tools.Config
import Control.Applicative hiding ((<|>), many)
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim
import Syntax.GeometricParser

instance LoopMode StreamMode where
  runOnce	  = streamRun
  enterMode = enterStream
  exitMode  = exitStream
  showHelp  = streamHelp
  modeTag   = streamTag

data StreamMode = StreamM

data TheoryCommand = Load TheoryFile
type TheoryFile = String

--------------------
-- Mode Functions --
--------------------
streamRun :: StreamMode -> REPLState -> String -> IO(Either Error REPLState)
streamRun mode state@(REPLState config theory gstar stream model) command = case parseTheoryCommand command of
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
enterStream :: StreamMode -> REPLState -> Either Error (REPLState, StreamMode)
enterStream mode state@(REPLState config theory gstar stream model) = Right (state, mode)

exitStream :: StreamMode -> IO()
exitStream mode = return ()

-----------------------
-- Command Functions --
-----------------------
streamTag :: StreamMode -> String
streamTag mode = "%stream% "

streamHelp :: StreamMode -> IO()
streamHelp cmd = prettyPrint 0 foutput $ ""++ 
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

