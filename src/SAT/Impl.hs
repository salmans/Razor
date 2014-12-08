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

module SAT.Impl ( SATTheoryType, SATIteratorType
                , satInitialize, satSolve, satClose, satNext, satAugment
                , storeSequent, blameSequent) where


-- Common
import Common.Model (Model)
import Common.Observation (ObservationSequent)
import Common.Provenance (Blame)


-- Control
import qualified Control.Monad.State.Lazy as State

-- SAT
import qualified SAT.Data (satInitialize, satSolve, satClose, satAugment
                          , storeSequent, blameSequent)

-- Tools
import Tools.Config (Config)

-- <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
-- Uncomment to select the implementation:

-- import SAT.Picosat -- Experimental implementation (Not working)
import SAT.SMTLib2
-- import SAT.SBV -- This implementation is outdated. The file will be deleted
                  -- soon.
-- <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

type SATIteratorState = State.State SATIteratorType

{-| Initializes the solver with the selected theory type and returns an iterator
  of the selected iterator type. -}
satInitialize :: Config -> SATTheoryType -> SATIteratorType
satInitialize = SAT.Data.satInitialize

{-| Closes the connection to the SMT solver if applicable. -}
satClose :: SATIteratorType -> ()
satClose =  SAT.Data.satClose

{-| Given an instance of the selected iterator type, returns a model (if a 
  solution exists) and an iterator for getting the next list of observations. -}
satSolve :: SATIteratorType -> (Maybe Model, SATIteratorType)
satSolve =  SAT.Data.satSolve

satNext :: SATIteratorType -> (Maybe Model, SATIteratorType)
satNext = satSolve

satAugment :: SATIteratorType -> (Maybe Model, SATIteratorType)
satAugment = SAT.Data.satAugment

{-| Adds an instance of 'ObservationSequent' to an instance of 'SATTheoryType'.
  This function is primarily used to implement augmentation. -}
storeSequent :: SATTheoryType -> (Blame, ObservationSequent) -> SATTheoryType
storeSequent =  SAT.Data.storeSequent

{-| Returns the observational sequent associated with the given blame
  This function is primarily used to implement provenance queries -}
blameSequent :: SATTheoryType -> Blame -> Maybe ObservationSequent
blameSequent = SAT.Data.blameSequent
