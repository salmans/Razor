{- This file is part of Razor.

  Razor is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Razor is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Razor.  If not, see <http://www.gnu.org/licenses/>.

  Module      : REPL.Mode.Explore
  Description : This module defines the Explore (Horizontal Model Exploration) Mode in the REPL
  Maintainer  : Salman Saghafi, Ryan Danas
-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module REPL.Mode.Explore where
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

instance LoopMode ExploreMode ExploreIn ExploreOut where
  runOnce	  = exploreRun
  update    = updateExplore
  enterMode = enterExplore

instance Mode ExploreMode where
  showHelp  = exploreHelp
  modeTag   = exploreTag

data ExploreMode = ExploreM
type ExploreIn = (Config, Theory, ChaseState, SATIteratorType, Model, [Formula])
type ExploreOut = (Theory, ChaseState, SATIteratorType, Model, [Formula])

data ExploreCommand = Current | Next | Push Formula | Pop

--------------------
-- Mode Functions --
--------------------
exploreRun :: ExploreMode -> ExploreIn -> String -> IO(Either Error ExploreOut)
exploreRun mode state@(config, theory, gstar, satdata, model, augs) command = case parseExploreCommand command of
  Left err -> return $ Left err
  Right cmd -> case cmd of
    Current -> do
      prettyModel $ Just model
      return $ Right $ (theory, gstar, satdata, model, augs)
    Next -> case modelNext (Right satdata) of
      Nothing -> return $ Left "No more minimal models in the stream!"
      Just (satdata', model') -> do
        prettyModel $ Just model'
        return $ Right $ (theory, gstar, satdata', model', augs)
    Push fml -> case modelUp config theory gstar fml of
      Nothing -> return $ Left $ "Given formula is not in the form of an augmentation!"
      Just gstar'@(b',p',t',c') -> case modelNext (Left (config, t')) of
        Nothing -> return $ Left "No models exist from adding the given augmentation!"
        Just (satdata', model') -> do
          prettyModel $ Just model'
          return $ Right $ (theory, gstar', satdata', model', fml:augs)
    Pop -> case augs of
      [] -> return $ Left $ "No pushed augmentations to pop!"
      _ -> case modelDown satdata of
        Nothing -> return $ Left $ "Somehow undoing the augmentation did not return a model!"
        Just (satdata', model') -> do
          prettyModel $ Just model'
          return $ Right $ (theory, gstar, satdata', model', (tail augs))

------------------------
-- RazorState Related --
------------------------
updateExplore :: ExploreMode -> ExploreOut -> RazorState -> (RazorState, ExploreIn)
updateExplore mode (theory', gstar', satdata', model', augs') state@(RazorState config theory gstar satdata model) = (RazorState config theory (Just gstar') (Just satdata') (Just model'), (config, theory', gstar', satdata', model', augs'))

enterExplore :: ExploreMode -> RazorState -> IO(Either Error ExploreOut)
enterExplore mode state@(RazorState config theory gstar satdata model) = case (theory, gstar, satdata, model) of
  (Just theory', Just gstar', Just satdata', Just model') -> do
    prettyModel $ Just model'
    return $ Right $ (theory', gstar', satdata', model', [])
  _ -> return $ Left "Modelspace not initialized by another mode!"

-------------
-- Helpers --
-------------

-----------------------
-- Command Functions --
-----------------------
exploreTag :: ExploreMode -> String
exploreTag mode = "%explore% "

exploreHelp :: ExploreMode -> IO()
exploreHelp cmd = prettyPrint 0 foutput $ ""++ 
  "<expr>:= | current            Display the current position in the modelspace\n"++
  "         | next               Display the next minimal model in the stream\n"++
  "         | aug <formula>      Augment the current model with the given formula\n"++
  "         | undo               Undo the most recent augmentation\n"

parseExploreCommand :: String -> Either Error ExploreCommand
parseExploreCommand cmd = 
	let pResult = parse pCommand "parsing EXPLORE command" cmd
	in case pResult of
		Left err -> Left $ show err
		Right val -> Right $ val

pCommand :: Parser ExploreCommand
pCommand = pCurrent <|> pNext <|> pPush <|> pPop

pCurrent :: Parser ExploreCommand
pCurrent = symbol "current" >> return Current

pNext :: Parser ExploreCommand
pNext = symbol "next" >> return Next

pPush :: Parser ExploreCommand
pPush = symbol "aug" >> Push <$> xpFactor

pPop :: Parser ExploreCommand
pPop = symbol "undo" >> return Pop
