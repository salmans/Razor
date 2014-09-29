{-| 
  Razor
  Module      : Chase.HerbrandBase.HerbrandBase
  Description : Provides a container for storing facts that the Chase 
  computes. This implementation for HerbrandBase is based on relational algebra.
  Maintainer  : Salman Saghafi -}

module Chase.HerbrandBase.RelAlg.HerbrandBase 
    ( ChaseSequentType, ChaseHerbrandBaseType ) where

import Chase.HerbrandBase.RelAlg.IHerbrandBase

type ChaseSequentType      = RelSequent
type ChaseHerbrandBaseType = RelHerbrandBase