{-| 
  Razor
  Module      : Common.Observation
  Description : The module defines 'Observation' and functions to work with it.
  'Observation' is the building block of models.
  Maintainer  : Salman Saghafi -}
module Common.Observation ( Observation (..), ObservationSequent (..) 
                          , buildObservationSequent, toObservation ) where

import Common.IObservation