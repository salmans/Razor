{-| 
  Razor
  Module      : Chase.HerbrandBase.RelAlg.Translate
  Description : Provides an interface to functions that are used to translate
  goemtric logic to relational algebra.
  Maintainer  : Salman Saghafi
-}
module Chase.HerbrandBase.RelAlg.Translate 
    ( bodyRelExp, headRelExp, delta
    , evaluateRelExp, evaluateRelExpNoDelta
    , tupleTransformer, insertTuples ) where

import Chase.HerbrandBase.RelAlg.ITranslate