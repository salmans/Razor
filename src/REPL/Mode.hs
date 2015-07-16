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
  
  Module      : REPL.Mode
  Description : This module defines what a REPL mode is
  Maintainer  : Salman Saghafi, Ryan Danas
-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module REPL.Mode where
import API.Surface
import Common.Model
import Common.Provenance
import Tools.Config
import Data.Maybe
import Data.List
import Syntax.GeometricUtils 
import SAT.Impl
import Chase.Impl

class (Mode m) => LoopMode m i o | m -> i o where
  runOnce     ::    m -> i -> String -> IO(Either Error o)
  update      ::    m -> o -> RazorState -> (RazorState, i)
  enterMode   ::    m -> RazorState -> IO(Either Error o)
  
class Mode m where
  showHelp    ::    m -> IO()
  modeTag     ::    m -> String
  check       ::    m -> String -> Bool
