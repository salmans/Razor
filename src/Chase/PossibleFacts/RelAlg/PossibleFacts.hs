{-| 
  Razor
  Module      : Chase.PossibleFacts.PossibleFacts
  Description : Provides a container for storing facts that the Chase 
  computes. This implementation for PossibleFacts is based on relational algebra.
  Maintainer  : Salman Saghafi -}

module Chase.PossibleFacts.RelAlg.PossibleFacts
    ( ChaseSequentType, ChasePossibleFactsType ) where

import Chase.PossibleFacts.RelAlg.IPossibleFacts

type ChaseSequentType       = RelSequent
type ChasePossibleFactsType = RelPossibleFacts