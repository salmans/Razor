{-| 
  Razor
  Module      : Common.Provenance
  Description : Defines provenance information and offers functions to work with
  provenance information.
  Maintainer  : Salman Saghafi -}
module Common.Provenance ( ProvInfo (..), ElementProvs
                         , addElementProv, getElementProv
                         , findElementWithProv, modifyElementProvs
                         , emptyProvInfo, emptyProvInfoWithConstants ) where

import Common.IProvenance