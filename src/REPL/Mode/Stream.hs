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

data StreamCommand = Current | Next

--------------------
-- Mode Functions --
--------------------
streamRun :: StreamMode -> StreamIn -> String -> IO(Either Error StreamOut)
streamRun mode state@(stream, model) command = case parseStreamCommand command of
  Left err -> return $ Left err
  Right cmd -> case cmd of
    Current -> do
      prettyModelWithIndex model
      return $ Right $ (stream, model)
    Next -> case modelNext (Right stream) of
      Nothing -> return $ Left "No more minimal models in the stream!"
      Just (stream', model') -> do
        prettyModelWithIndex model'
        return $ Right $ (stream', model')

------------------------
-- RazorState Related --
------------------------
updateStream :: StreamMode -> StreamOut -> RazorState -> (RazorState, StreamIn)
updateStream mode (stream', model') state@(RazorState config theory gstar stream model) = (RazorState config theory gstar (Just stream') (Just model'), (stream', model'))

enterStream :: StreamMode -> RazorState -> IO(Either Error StreamOut)
enterStream mode state@(RazorState config theory gstar stream model) = case (stream, model) of
  (Just stream', Just model') -> do
    prettyModelWithIndex model'
    return $ Right $ (stream', model')
  (_, _) -> return $ Left "Model stream not initialized by another mode!"

-------------
-- Helpers --
-------------
prettyModelWithIndex :: Model -> IO()
prettyModelWithIndex model = do
  prettyModel (Just model)

-----------------------
-- Command Functions --
-----------------------
streamTag :: StreamMode -> String
streamTag mode = "%stream% "

streamHelp :: StreamMode -> IO()
streamHelp cmd = prettyPrint 0 foutput $ ""++ 
  "<expr>:= | current     Display the current minimal model in the stream\n"++
  "         | next        Display the next minimal model in the stream\n"

parseStreamCommand :: String -> Either Error StreamCommand
parseStreamCommand cmd = 
	let pResult = parse pCommand "parsing STREAM command" cmd
	in case pResult of
		Left err -> Left $ show err
		Right val -> Right $ val

pCommand :: Parser StreamCommand
pCommand = pCurrent <|> pNext

pCurrent :: Parser StreamCommand
pCurrent = symbol "current" >> return Current

pNext :: Parser StreamCommand
pNext = symbol "next" >> return Next
