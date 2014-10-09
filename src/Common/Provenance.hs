{-| 
  Razor
  Module      : Common.Provenance
  Description : Defines provenance information and offers functions to work with
  provenance information.
  Maintainer  : Salman Saghafi -}
module Common.Provenance ( ProvInfo (..), ElementProvs, Blame (..)
                         , addElementProv, getElementProv
                         , findElementWithProv, modifyElementProvs
                         , findObservationWithProv
                         , emptyProvInfo, emptyProvInfoWithConstants ) where

import Common.IProvenance