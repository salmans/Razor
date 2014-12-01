{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
  Razor
  Module      : REPL.Mode.Model
  Description : This module defines the implicit first model check mode in the REPL.
  Maintainer  : Salman Saghafi, Ryan Danas
-}
module REPL.Mode.Model where
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

instance LoopMode ModelMode ModelIn ModelOut where
  runOnce	  = modelRun
  update    = updateModel
  enterMode = enterModel

instance Mode ModelMode where
  showHelp  = modelHelp
  modeTag   = modelTag

data ModelMode = ModelM
type ModelIn = ()
type ModelOut = (SATIteratorType, Model)

--------------------
-- Mode Functions --
--------------------
modelRun :: ModelMode -> ModelIn -> String -> IO(Either Error ModelOut)
modelRun mode _ command = return $ Left "no commands"

------------------------
-- RazorState Related --
------------------------
updateModel :: ModelMode -> ModelOut -> RazorState -> (RazorState, ModelIn)
updateModel mode (stream', model') state@(RazorState config theory gstar stream model) = (RazorState config theory gstar (Just stream') (Just model'), ())

enterModel :: ModelMode -> RazorState -> IO(Either Error ModelOut)
enterModel mode state@(RazorState config theory gstar stream model) = do
  case gstar of
    Nothing -> return $ Left $ "No theory loaded!"
    Just (b,p,t,c) -> case modelNext $ Left (config, t) of
      Nothing -> return $ Left "No models available!"
      Just (stream', model') -> return $ Right $ (stream', model')

-----------------------
-- Command Functions --
-----------------------
modelTag :: ModelMode -> String
modelTag mode = "%model% "

modelHelp :: ModelMode -> IO()
modelHelp cmd = prettyPrint 0 foutput $ "no commands"
