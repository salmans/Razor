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

  Module      : REPL.Mode.ModelCheck
  Description : This module defines the implicit first model check mode in the REPL.
  Maintainer  : Salman Saghafi, Ryan Danas
-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module REPL.Mode.ModelCheck where
import Control.Applicative hiding ((<|>), many)
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim
import Syntax.GeometricParser
import API.Surface
import Common.Model
import SAT.Impl
import Data.Map
import REPL.Mode
import REPL.Display
import Common.Model
import Syntax.GeometricUtils
import Tools.Config

instance LoopMode ModelCheckMode ModelCheckIn ModelCheckOut where
  runOnce	  = modelRun
  update    = updateModelCheck
  enterMode = enterModelCheck

instance Mode ModelCheckMode where
  showHelp  = modelHelp
  modeTag   = modelTag

data ModelCheckMode = ModelCheckM
type ModelCheckIn = ()
type ModelCheckOut = (Config, Theory, ModelSpace, ModelCoordinate)

--------------------
-- Mode Functions --
--------------------
modelRun :: ModelCheckMode -> ModelCheckIn -> String -> IO(Either Error ModelCheckOut)
modelRun mode _ command = return $ Left "no commands"

------------------------
-- RazorState Related --
------------------------
updateModelCheck :: ModelCheckMode -> ModelCheckOut -> RazorState -> (RazorState, ModelCheckIn)
updateModelCheck mode (config', theory', mspace', mcoor') state@(RazorState config theory mspace mcoor) = (RazorState config' (Just theory') mspace' (Just mcoor'), ())

enterModelCheck :: ModelCheckMode -> RazorState -> IO(Either Error ModelCheckOut)
enterModelCheck mode state@(RazorState config theory mspace mcoor) = case theory of
  Just thy -> firstModel config thy
  _ -> do
    load <- loadTheory config False
    case load of
      Left err -> return $ Left err
      Right (cfg, thy) -> firstModel cfg thy
  where
    firstModel config theory = do
      let chasestate = chaseTheory config theory
      case modelNext (Left chasestate) mspace of
        Nothing -> return $ Left $ "No models available!"
        Just (mspace', mcoor') -> return $ Right $ (config, theory, mspace', mcoor')

-----------------------
-- Command Functions --
-----------------------
modelTag :: ModelCheckMode -> String
modelTag mode = "incomplete"

modelHelp :: ModelCheckMode -> IO()
modelHelp cmd = prettyPrint 0 foutput $ "no commands"
