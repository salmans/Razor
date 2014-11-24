{-| 
  Razor
  Module      : Common.Provenance
  Description : Defines provenance information and offers functions to work with
  provenance information.
  Maintainer  : Salman Saghafi -}
module Common.Provenance ( Blame (..), ObservationProvs, ProvInfo (..)
                         , ElementProvs
                         , addElementProv, getElementProv
                         , findElementWithProv, modifyElementProvs
                         , findObservationWithProv
                         , emptyProvInfo, emptyProvInfoWithConstants
                         , SkolemTerm ) where

import Common.IProvenance