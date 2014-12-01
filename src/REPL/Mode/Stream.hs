{-|
  Razor
  Module      : REPL.Mode.Stream
  Description : This module defines the Stream (Horizontal Model Exploration) Mode in the REPL
  Maintainer  : Salman Saghafi, Ryan Danas
-}
module REPL.Mode.Stream where
import API.Surface
import Common.Model
import SAT.Impl
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
  showHelp  = streamHelp
  modeTag   = streamTag

data StreamMode = StreamM

data StreamCommand = Next

--------------------
-- Mode Functions --
--------------------
streamRun :: StreamMode -> RazorState -> String -> IO(Either Error RazorState)
streamRun mode state@(RazorState config theory gstar stream model) command = case parseStreamCommand command of
  Left err -> return $ Left err
  Right cmd -> case cmd of
    Next -> return $ Right state

runNext :: Either Error (SATIteratorType, Model) -> RazorState -> IO(Either Error RazorState)
runNext next state@(RazorState config theory gstar stream model) = case next of
  Left err -> return $ Left err
  Right (stream', model') -> do
    prettyModel (Just model')
    return $ Right (RazorState config theory gstar (Just stream') (Just model'))

---------------------
-- chmod Functions --
---------------------
enterStream :: StreamMode -> RazorState -> IO(Either Error (RazorState, StreamMode))
enterStream mode state@(RazorState config theory gstar stream model) = do
  s' <- runNext next state
  case s' of
    Left err -> return $ Left err
    Right state' -> return $ Right (state', mode)
  where
    next = case (stream, model) of
      (Just str, Just mdl) -> Right (str, mdl)
      (Just str, _) -> modelNext $ Right str
      (Nothing, _) -> case gstar of
        Nothing -> Left $ "No theory loaded!"
        Just gs -> modelNext $ Left (config, gs)

-----------------------
-- Command Functions --
-----------------------
streamTag :: StreamMode -> String
streamTag mode = "%stream% "

streamHelp :: StreamMode -> IO()
streamHelp cmd = prettyPrint 0 foutput $ ""++ 
  "<expr>:= |   next       Load the given filename as an input theory"

parseStreamCommand :: String -> Either Error StreamCommand
parseStreamCommand cmd = 
	let pResult = parse pCommand "parsing THEORYMODE command" cmd
	in case pResult of
		Left err -> Left $ show err
		Right val -> Right $ val

pCommand :: Parser StreamCommand
pCommand = pNext

pNext :: Parser StreamCommand
pNext = do
	symbol "next"
	spaces
	return $ Next
