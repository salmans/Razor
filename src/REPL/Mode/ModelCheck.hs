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
import API.Surface
import Common.Model
import SAT.Impl
import Data.Map
import REPL.Mode
import REPL.Display
import Tools.Config
import Control.Applicative hiding ((<|>), many)
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim
import Syntax.GeometricParser

instance LoopMode ModelCheckMode ModelCheckIn ModelCheckOut where
  runOnce	  = modelRun
  update    = updateModelCheck
  enterMode = enterModelCheck

instance Mode ModelCheckMode where
  showHelp  = modelHelp
  modeTag   = modelTag

data ModelCheckMode = ModelCheckM
type ModelCheckIn = ()
type ModelCheckOut = (ModelSpace, ModelCoordinate)

--------------------
-- Mode Functions --
--------------------
modelRun :: ModelCheckMode -> ModelCheckIn -> String -> IO(Either Error ModelCheckOut)
modelRun mode _ command = return $ Left "no commands"

------------------------
-- RazorState Related --
------------------------
updateModelCheck :: ModelCheckMode -> ModelCheckOut -> RazorState -> (RazorState, ModelCheckIn)
updateModelCheck mode (mspace', mcoor') state@(RazorState config theory gstar mspace mcoor) = (RazorState config theory gstar mspace' (Just mcoor'), ())

enterModelCheck :: ModelCheckMode -> RazorState -> IO(Either Error ModelCheckOut)
enterModelCheck mode state@(RazorState config theory gstar mspace mcoor) = do
  case gstar of
    Nothing -> return $ Left $ "No theory loaded!"
    Just (b,p,t,c) -> case modelNext $ Left (config, t) of
      Nothing -> return $ Left "No models available!"
      Just (mspace', mcoor') -> return $ Right $ (mspace', mcoor')

-----------------------
-- Command Functions --
-----------------------
modelTag :: ModelCheckMode -> String
modelTag mode = "%incomplete% "

modelHelp :: ModelCheckMode -> IO()
modelHelp cmd = prettyPrint 0 foutput $ "no commands"
