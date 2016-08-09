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

  Module      : SAT.IData
  Description : The module defines the basic datatypes that provide an 
  interface to the SAT-solving modules.
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module SAT.Data where

-- Standard
import qualified Data.Map as Map
import Data.List (intercalate)

-- Control
import Control.Applicative
import Control.DeepSeq
import qualified Control.Monad.State.Lazy as State

-- Common
import Common.Data (SequentLike (..))
import Common.Observation (Observation, ObservationSequent)
import Common.Provenance (Blame)
import Common.Model (Model)

-- Tools
import Tools.Config (Config)


{-| SATIterator is the class of types that can act as an iterator for SAT/SMT
  solving. 

  [@satInitialize@] constructs a SAT/SMT iterator.
  [@satStor@] stores an observational sequent as extra constraints into the 
  given iterator.
  [@satSolve@] returns a next 'Model' (if exists) for a given iterator and
  a new iterator for fetching the next model.
  [@satAugment@]
  [@satBacktrack@]
  [@satPush@] makes a call to the @push@ function of the underlying SMT solver
  or fakes a push action if the connection to the solver is not incremental.
  [@satPop@] makes a call to the @pop@ function of the underlying SMT solver
  or fakes a pop action if the connection to the solver is not incremental.
  [@satClose@] closes the connection to the SMT solver.
-}
class SATIterator i where
    satInitialize :: Config -> i
    satStore      :: ObservationSequent -> i -> i
    satSolve      :: i -> (Maybe Model, i)
    satAugment    :: i -> (Maybe Model, i)
    satBacktrack  :: i -> (Maybe Model, i)
    satPush       :: i -> i
    satPop        :: i -> i
    satClose      :: i -> ()
