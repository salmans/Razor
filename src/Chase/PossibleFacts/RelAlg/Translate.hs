{-| 
  Razor
  Module      : Chase.PossibleFacts.RelAlg.Translate
  Description : Provides an interface to functions that are used to translate
  goemtric logic to relational algebra.
  Maintainer  : Salman Saghafi
-}
module Chase.PossibleFacts.RelAlg.Translate 
    ( bodyRelExp, headRelExp, delta, evaluateRelExp
    , insertTuples, tupleTransformer ) where

import Chase.PossibleFacts.RelAlg.ITranslate