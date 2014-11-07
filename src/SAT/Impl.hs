{-|
  Razor
  Module      : SAT.Impl
  Description : The module provides the interface to a particular implementation
  of SAT/SMT solving.
  Maintainer  : Salman Saghafi -}

module SAT.Impl ( SATTheoryType, SATIteratorType, 
                  satInitialize, satSolve, storeSequent) where 


-- Common
import Common.Model (Model)
import Common.Observation (ObservationSequent)

-- Control
import qualified Control.Monad.State.Lazy as State

-- SAT
import qualified SAT.Data (satInitialize, satSolve, storeSequent)

-- <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
-- Uncomment to select the implementation:

-- import SAT.Picosat -- Not working
import SAT.SMT
-- <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

type SATIteratorState = State.State SATIteratorType

{-| Initializes the solver with the selected theory type and returns an iterator
  of the selected iterator type. -}
satInitialize :: SATTheoryType -> SATIteratorType
satInitialize = SAT.Data.satInitialize

{-| Given an instance of the selected iterator type, returns a model (if a 
  solution exists) and an iterator for getting the next list of observations. -}
satSolve :: SATIteratorType -> (Maybe Model, SATIteratorType)
satSolve =  SAT.Data.satSolve

{-| Adds an instance of 'ObservationSequent' to an instance of 'SATTheoryType'.
  This function is primarily used to implement augmentation. -}
storeSequent :: SATTheoryType -> ObservationSequent -> SATTheoryType
storeSequent =  SAT.Data.storeSequent