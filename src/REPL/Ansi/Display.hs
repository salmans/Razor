{-| 
  Razor
  Module      : REPL.Ansi.Display
  Description : Displays various data-structures.
  Maintainer  : Salman Saghafi -}

module REPL.Ansi.Display (displayInit, displayExit, prettyPrint, prettyHighlight, fmodel, fhigh, finfo, fwarning, ferror) where

import REPL.Ansi.IDisplay