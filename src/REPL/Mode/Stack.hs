{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
  Razor
  Module      : REPL.Mode.Stack
  Description : This module defines the Stack (Vertical Model Exploration) Mode in the REPL
  Maintainer  : Salman Saghafi, Ryan Danas
-}
module REPL.Mode.Stack where
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
import Syntax.GeometricUtils
import Data.List
import Syntax.IGeometric

instance LoopMode StackMode StackIn StackOut where
  runOnce	  = stackRun
  update    = updateStack
  enterMode = enterStack

instance Mode StackMode where
  showHelp  = stackHelp
  modeTag   = stackTag

data StackMode = StackM
type StackIn = (Config, Theory, GStar, SATIteratorType, Model, [Formula])
type StackOut = (Theory, GStar, SATIteratorType, Model, [Formula])

data StackCommand = Current | Push Formula | Pop

--------------------
-- Mode Functions --
--------------------
stackRun :: StackMode -> StackIn -> String -> IO(Either Error StackOut)
stackRun mode state@(config, theory, gstar, stack, model, augs) command = case parseStackCommand command of
  Left err -> return $ Left err
  Right cmd -> case cmd of
    Current -> do
      prettyModelWithStack model augs
      return $ Right $ (theory, gstar, stack, model, augs)
    Push fml -> case modelUp config theory gstar fml of
      Nothing -> return $ Left $ "Given formula is not in the form of an augmentation!"
      Just gstar'@(b',p',t',c') -> case modelNext (Left (config, t')) of
        Nothing -> return $ Left "No models exist from adding the given augmentation!"
        Just (stack', model') -> do
          prettyModelWithStack model' (fml:augs)
          return $ Right $ (theory, gstar', stack', model', fml:augs)
    Pop -> case augs of
      [] -> return $ Left $ "No pushed augmentations to pop!"
      _ -> case modelDown stack of
        Nothing -> return $ Left $ "Somehow undoing the augmentation did not return a model!"
        Just (stack', model') -> do
          prettyModelWithStack model' (tail augs)
          return $ Right $ (theory, gstar, stack', model', (tail augs))

------------------------
-- RazorState Related --
------------------------
updateStack :: StackMode -> StackOut -> RazorState -> (RazorState, StackIn)
updateStack mode (theory', gstar', stack', model', augs') state@(RazorState config theory gstar stack model) = (RazorState config theory gstar (Just stack') (Just model'), (config, theory', gstar', stack', model', augs'))

enterStack :: StackMode -> RazorState -> IO(Either Error StackOut)
enterStack mode state@(RazorState config theory gstar stack model) = case (theory, gstar, stack, model) of
  (Just theory', Just gstar', Just stack', Just model') -> do
    prettyModelWithStack model' []
    return $ Right $ (theory', gstar', stack', model', [])
  _ -> return $ Left "Model stack not initialized by another mode!"

-------------
-- Helpers --
-------------
prettyModelWithStack :: Model -> [Formula] -> IO()
prettyModelWithStack model augs = do
  prettyModel (Just model)
  prettyPrint 0 foutput $ "TOP Augmentation Stack TOP\n"
  mapM_ (\aug->prettyPrint 0 finput ((show aug)++"\n")) augs
  prettyPrint 0 foutput $ "BTM Augmentation Stack BTM"

-----------------------
-- Command Functions --
-----------------------
stackTag :: StackMode -> String
stackTag mode = "%stack% "

stackHelp :: StackMode -> IO()
stackHelp cmd = prettyPrint 0 foutput $ ""++ 
  "<expr>:= | current           Display the model and augmentations at the top of the stack\n"++
  "         | push <formula>    Augment the current model with the given formula\n"++
  "         | pop               Undo the most recently pushed augmentation"

parseStackCommand :: String -> Either Error StackCommand
parseStackCommand cmd = 
	let pResult = parse pCommand "parsing STACK command" cmd
	in case pResult of
		Left err -> Left $ show err
		Right val -> Right $ val

pCommand :: Parser StackCommand
pCommand = pPush +++ pPop

pPush :: Parser StackCommand
pPush = symbol "push" >> Push <$> xpFactor

pPop :: Parser StackCommand
pPop = symbol "pop" >> return Pop
