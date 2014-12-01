{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

instance LoopMode StreamMode StreamIn StreamOut where
  runOnce	  = streamRun
  update    = updateStream
  enterMode = enterStream

instance Mode StreamMode where
  showHelp  = streamHelp
  modeTag   = streamTag

data StreamMode = StreamM
type StreamIn = (SATIteratorType, Model)
type StreamOut = (SATIteratorType, Model)

data StreamCommand = Next

--------------------
-- Mode Functions --
--------------------
streamRun :: StreamMode -> StreamIn -> String -> IO(Either Error StreamOut)
streamRun mode state@(stream, model) command = case parseStreamCommand command of
  Left err -> return $ Left err
  Right cmd -> case cmd of
    Next -> case modelNext (Right stream) of
      Left err -> return $ Left err
      Right (stream', model') -> do
        prettyModel (Just model')
        return $ Right $ (stream', model')

------------------------
-- RazorState Related --
------------------------
updateStream :: StreamMode -> StreamOut -> RazorState -> (RazorState, StreamIn)
updateStream mode (stream', model') state@(RazorState config theory gstar stream model) = (RazorState config theory gstar (Just stream') (Just model'), (stream', model'))

enterStream :: StreamMode -> RazorState -> IO(Either Error StreamOut)
enterStream mode state@(RazorState config theory gstar stream model) = do
  case next of
    Left err -> return $ Left err
    Right (stream', model') -> do
      prettyModel (Just model')
      return $ Right $ (stream', model')
  where
    next = case (stream, model) of
      (Just str, Just mdl) -> Right (str, mdl)
      (Just str, Nothing) -> modelNext $ Right str
      (Nothing, _) -> case gstar of
        Nothing -> Left $ "No theory loaded!"
        Just (b,p,t,c) -> modelNext $ Left (config, t)

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
