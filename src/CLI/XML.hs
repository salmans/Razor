{-|
  Razor
  Module      : CLI.XML
  Description : The module provides a XML pickling / depicking for the I/O data structures of the command line interface. 
  Maintainer  : Salman Saghafi, Ryan Danas
-}

module CLI.XML where
import Text.XML.HXT.Arrow.Pickle
import Text.XML.HXT.Core

toXML :: a -> String
toXML struct = showPickled [] struct

