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
import Text.ParserCombinators.Parsec hiding (try)
import Text.Parsec.Prim hiding (try)
import Syntax.GeometricUtils
import Data.List
import Syntax.IGeometric
import System.Process
import GHC.IO.Handle
import Control.Exception

instance LoopMode ExploreMode ExploreIn ExploreOut where
  runOnce         = exploreRun
  update    = updateExplore
  enterMode = enterExplore

instance Mode ExploreMode where
  showHelp  = exploreHelp
  modeTag   = exploreTag
  check m cmd = case parseExploreCommand cmd of
    Left err -> False
    _ -> True

data ExploreMode = ExploreM
type ExploreIn = (Config, Theory, ModelSpace, Maybe ModelCoordinate)
type ExploreOut = (Theory, ModelSpace, Maybe ModelCoordinate)

data ExploreCommand = Viz | Current | Next | Push Formula | Pop

--------------------
-- Mode Functions --
--------------------
exploreRun :: ExploreMode -> ExploreIn -> String -> IO(Either Error ExploreOut)
exploreRun mode state@(config, theory, mspace, mcoor) command = case parseExploreCommand command of
  Left err -> return $ Left err
  Right cmd -> case mcoor of 
    -- first model case
    Nothing -> case cmd of
      Next -> do
        let chasestate = chaseTheory config theory
        case modelNext (Left chasestate) mspace of
          Nothing -> return $ Left "No minimal models for the given theory!\n"
          Just (mspace', mcoor') -> do
            prettyModel $ modelLookup mspace' (Just mcoor')
            return $ Right $ (theory, mspace', Just mcoor')
      _ -> return $ Left "No current model!\n"
    -- after first model case
    Just mcoor' -> case cmd of
      Viz -> do
        xmlModel "model.xml" $ modelLookup mspace (Just mcoor')
        runViz "model.xml"
        return $ Right $ (theory, mspace, Just mcoor')
      Current -> do
        prettyModel $ modelLookup mspace (Just mcoor')
        prettyModelCoordinate mcoor'
        return $ Right $ (theory, mspace, Just mcoor')
      Next -> case modelNext (Right mcoor') mspace of
        Nothing -> return $ Left "No minimal models for the given theory!"
        Just (mspace', mcoor'') -> do
          case (mcoor'==mcoor'') of
            True -> prettyPrint 0 ferror "No more minimal models for the given theory!\n"
            False -> prettyModel $ modelLookup mspace' (Just mcoor'')
          return $ Right $ (theory, mspace', Just mcoor'')
      Push fml -> case modelspaceLookup mspace mcoor' of
        Nothing -> return $ Left "No current model!"
        Just (_, model) -> case getAugmentation model fml of
          Nothing -> return $ Left $ "Given formula is not in the form of an augmentation!"
          Just (obs,newelms) -> case modelUp config theory (obs,newelms) mspace mcoor' of
            Nothing -> return $ Left "No models exist from adding the given augmentation!"
            Just (mspace', mcoor'') -> do
              prettyModel $ modelLookup mspace' (Just mcoor'')
              return $ Right $ (theory, mspace', Just mcoor'')
      Pop -> case lastAug mcoor' of
        Nothing -> return $ Left "No augmentation in explore history to undo!"
        Just (obs, mcoor'') -> do
          prettyPrint 0 foutput $ "Undoing last augmentation "++(show obs)++"...\n"
          case modelDown mspace mcoor' mcoor'' of
            Nothing -> return $ Left "Could not undo previous augmentation!"
            Just mspace' -> do
              prettyModel $ modelLookup mspace' (Just mcoor'')
              return $ Right $ (theory, mspace', Just mcoor'')
        where
          lastAug (Stack obs mcoor'') = Just (obs, mcoor'')
          lastAug (Stream mcoor'') = lastAug mcoor''
          lastAug (Origin) = Nothing

            
------------------------
-- RazorState Related --
------------------------
updateExplore :: ExploreMode -> ExploreOut -> RazorState -> (RazorState, ExploreIn)
updateExplore mode (theory', mspace', mcoor') state@(RazorState config theory mspace mcoor) = (RazorState config theory mspace' mcoor', (config, theory', mspace', mcoor'))

enterExplore :: ExploreMode -> RazorState -> IO(Either Error ExploreOut)
enterExplore mode state@(RazorState config theory mspace mcoor) = case theory of
  -- Nothing loaded; return error
  Nothing -> return $ Left $ "Theory not loaded!\n"
  -- Theory loaded; enter mode
  Just theory' -> return $ Right $ (theory', mspace, mcoor)

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
  Stream Origin -> prettyPrint 0 foutput "-------------------\n"
  Stream mcoor' -> do
    prettyPrint 0 foutput "next\n"
    prettyModelCoordinatePlus mcoor'
  Stack obs mcoor' -> do
    prettyPrint 0 foutput ("aug "++(show obs)++"\n")
    prettyModelCoordinatePlus mcoor'

runViz :: String -> IO ()
runViz modelfile = do
  res <- try (createProcess (proc "java" ["-jar", "RazorViz.jar", modelfile])) :: IO (Either SomeException ((Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)))
  case res of
    Left ex -> prettyPrint 0 ferror "Could not run visualization jar file!\n" >> return ()
    _ -> return ()

-----------------------
-- Command Functions --
-----------------------
exploreTag :: ExploreMode -> String
exploreTag mode = "explore"

exploreHelp :: ExploreMode -> IO()
exploreHelp cmd = prettyPrint 0 foutput $ ""++ 
  "viz              Visualize the current model as an alloy instance\n"++ 
  "current          Display the explore history and current position in the modelspace\n"++
  "next             Get the next minimal model in the stream\n"++
  "aug R(e^#, ...)  Augment the current model with the given formula\n"++
  "undo             Undo the most recent augmentation\n"

parseExploreCommand :: String -> Either Error ExploreCommand
parseExploreCommand cmd = 
        let pResult = parse pCommand "parsing EXPLORE command" cmd
        in case pResult of
                Left err -> Left $ show err
                Right val -> Right $ val

pCommand :: Parser ExploreCommand
pCommand = pViz <|> pCurrent <|> pNext <|> pPush <|> pPop

pViz :: Parser ExploreCommand
pViz = string "viz" >> return Viz

pCurrent :: Parser ExploreCommand
pCurrent = string "current" >> return Current

pNext :: Parser ExploreCommand
pNext = string "next" >> return Next

pPush :: Parser ExploreCommand
pPush = string "aug" >> spaces >> Push <$> xpFactor

pPop :: Parser ExploreCommand
pPop = string "undo" >> return Pop
