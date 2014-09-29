{-|
  Razor
  Module      : SAT.Data
  Description : The module defines the basic datatypes that provide an 
  interface to the SAT-solving modules.
  Maintainer  : Salman Saghafi -}

module SAT.Data ( SATAtom (..), SATSequent(..), SATTheory (..)
                , SATSolver (..)-- , SATSolverM
                ) where


import SAT.IData