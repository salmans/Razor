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

  Module      : SAT.Impl
  Description : The module provides the interface to a particular implementation
  of SAT/SMT solving.
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

module SAT.Impl ( SATIteratorType
                , satInitialize, satStore, satSolve, satClose
                , satAugment, satPush, satPop, satBacktrack) where


-- Common
import Common.Model (Model)
import Common.Observation (ObservationSequent)
import Common.Provenance (Blame)


-- Control
import qualified Control.Monad.State.Lazy as State

-- SAT
import qualified SAT.Data (satInitialize, satStore, satSolve, satClose, satPush, satPop, satAugment, satBacktrack)

-- Tools
import Tools.Config (Config)

-- <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
-- Uncomment to select the implementation (or you would, if there were in
-- fact multiple working implementations; non-working code for Picosat and
-- SBV was removed):

import SAT.SMTLib2

-- <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

type SATIteratorState = State.State SATIteratorType

{-| Initializes the solver with the selected theory type and returns an iterator
  of the selected iterator type. -}
satInitialize :: Config -> SATIteratorType
satInitialize = SAT.Data.satInitialize

{-| Stores an 'ObservationSequent' as an extra constraint in the given 
    iterator -}
satStore :: ObservationSequent -> SATIteratorType -> SATIteratorType
satStore  = SAT.Data.satStore

{-| Closes the connection to the SMT solver if applicable. -}
satClose :: SATIteratorType -> ()
satClose =  SAT.Data.satClose

{-| Given an instance of the selected iterator type, returns a model (if a 
  solution exists) and an iterator for getting the next list of observations. -}
satSolve :: SATIteratorType -> (Maybe Model, SATIteratorType)
satSolve =  SAT.Data.satSolve

satPush :: SATIteratorType -> SATIteratorType
satPush = SAT.Data.satPush

satPop :: SATIteratorType -> SATIteratorType
satPop = SAT.Data.satPop

satAugment :: SATIteratorType -> (Maybe Model, SATIteratorType)
satAugment = SAT.Data.satAugment

satBacktrack :: SATIteratorType -> (Maybe Model, SATIteratorType)
satBacktrack = SAT.Data.satBacktrack
