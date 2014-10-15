{-| 
  Razor
  Module      : REPL.Ansi.Display
  Description : Displays various data-structures.
  Maintainer  : Salman Saghafi -}

module REPL.Ansi.Display (displayInit, displayExit, prettyPrint, prettyHighlight, finput, foutput, flow, fhigh, ferror) where

import REPL.Ansi.IDisplay