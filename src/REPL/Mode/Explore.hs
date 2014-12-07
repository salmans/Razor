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
import qualified Data.Map as Map
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
type ExploreIn = (Config, Theory, ChaseState, ModelSpace, ModelCoordinate)
type ExploreOut = (Theory, ChaseState, ModelSpace, ModelCoordinate)

data ExploreCommand = Current | Next | Prev | Push Formula | Pop

--------------------
-- Mode Functions --
--------------------
exploreRun :: ExploreMode -> ExploreIn -> String -> IO(Either Error ExploreOut)
exploreRun mode state@(config, theory, gstar, mspace, mcoor) command = case parseExploreCommand command of
  Left err -> return $ Left err
  Right cmd -> case cmd of
    Current -> do
      prettyModel $ modelLookup mspace (Just mcoor)
      prettyModelCoordinate mcoor
      return $ Right $ (theory, gstar, mspace, mcoor)
    Next -> case modelspaceLookup mspace (Stream mcoor) of
      Just (_, _, model) -> do
        prettyModel $ Just model
        prettyModelCoordinate (Stream mcoor)
        return $ Right $ (theory, gstar, mspace, (Stream mcoor))
      Nothing -> case modelspaceLookup mspace mcoor of 
        Nothing -> return $ Left "Current modelspace coordinate does not exist?!"
        Just _ -> case modelNext (Right (mspace, mcoor)) of
          Nothing -> return $ Left "No more minimal models in the stream!"
          Just (mspace', mcoor') -> do
            prettyModel $ modelLookup mspace' (Just mcoor')
            prettyModelCoordinate mcoor'
            return $ Right $ (theory, gstar, mspace', mcoor')
    Prev -> case mcoor of
      Stream mcoor' -> case modelspaceLookup mspace mcoor' of
        Just (_, _, model) -> do
          prettyModel $ Just model
          prettyModelCoordinate mcoor'
          return $ Right $ (theory, gstar, mspace, mcoor')
        Nothing -> return $ Left "No exploration to undo!"
      _ -> return $ Left "Last exploration in history was not 'next'!"
    Push fml -> case getObservation fml of
      Nothing -> return $ Left $ "Given formula is not in the form of an augmentation!"
      Just obs -> case modelspaceLookup mspace (Stack obs mcoor) of
        Just (_, _, model) -> do
          prettyModel $ Just model
          prettyModelCoordinate (Stack obs mcoor)
          return $ Right $ (theory, gstar, mspace, (Stack obs mcoor))
        Nothing -> case modelUp config theory gstar obs (mspace, mcoor) of
          Nothing -> return $ Left "No models exist from adding the given augmentation!"
          Just (gstar', mspace', mcoor') -> do
              prettyModel $ modelLookup mspace' (Just mcoor')
              prettyModelCoordinate mcoor'
              return $ Right $ (theory, gstar', mspace', mcoor')
    Pop -> case mcoor of
      Stack obs mcoor' -> case modelspaceLookup mspace mcoor of
        Nothing -> error "current model coordinate does not exist"
        Just (gs', _, _) -> case gs' of
          Nothing -> error "unable to undo augmentation; missing chasestate before augmentation was applied"
          Just gstar' -> case modelspaceLookup mspace mcoor' of
            Nothing -> return $ Left "No exploration to undo!"
            Just (_,_,model) -> do
              prettyModel $ Just model
              prettyModelCoordinate mcoor'
              return $ Right $ (theory, gstar', mspace, mcoor')
      _ -> return $ Left "Last exploration in history was not 'aug'!"
            
------------------------
-- RazorState Related --
------------------------
updateExplore :: ExploreMode -> ExploreOut -> RazorState -> (RazorState, ExploreIn)
updateExplore mode (theory', gstar', mspace', mcoor') state@(RazorState config theory gstar mspace mcoor) = (RazorState config theory (Just gstar') mspace' (Just mcoor'), (config, theory', gstar', mspace', mcoor'))

enterExplore :: ExploreMode -> RazorState -> IO(Either Error ExploreOut)
enterExplore mode state@(RazorState config theory gstar mspace mcoor) = case (theory, gstar, mcoor) of
  (Just theory', Just gstar', Just mcoor') -> case Map.lookup mcoor' mspace of
    Nothing -> return $ Left "Modelspace not initialized by another mode!"
    Just (_, _, model') -> do
      prettyModel $ Just model'
      prettyModelCoordinate mcoor'
      return $ Right $ (theory', gstar', mspace, mcoor')
  _ -> return $ Left "Modelspace not initialized by another mode!"

-------------
-- Helpers --
-------------
prettyModelCoordinate :: ModelCoordinate -> IO()
prettyModelCoordinate mcoor = do
  prettyPrint 0 foutput "Exploration History\n"
  prettyPrint 0 foutput "-------------------\n"
  prettyModelCoordinatePlus mcoor

prettyModelCoordinatePlus :: ModelCoordinate -> IO()
prettyModelCoordinatePlus mcoor = case mcoor of
  Stream mcoor' -> do
    prettyPrint 0 foutput "next\n"
    prettyModelCoordinatePlus mcoor'
  Stack obs mcoor' -> do
    prettyPrint 0 foutput ("aug "++(show obs)++"\n")
    prettyModelCoordinatePlus mcoor'
  Origin -> prettyPrint 0 foutput "-------------------\n"
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
pCommand = pCurrent <|> pNext <|> pPrev <|> pPush <|> pPop

pCurrent :: Parser ExploreCommand
pCurrent = symbol "current" >> return Current

pNext :: Parser ExploreCommand
pNext = symbol "next" >> return Next

pPrev :: Parser ExploreCommand
pPrev = symbol "prev" >> return Prev

pPush :: Parser ExploreCommand
pPush = symbol "aug" >> Push <$> xpFactor

pPop :: Parser ExploreCommand
pPop = symbol "undo" >> return Pop
