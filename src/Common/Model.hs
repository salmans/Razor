{-| 
  Razor
  Module      : Common.Model
  Description : Model is a data-structure to contain the models of a theory.
  Maintainer  : Salman Saghafi -}
module Common.Model ( Model (modelElements, modelObservations)
                    , emptyModel, createModel ) where

import Common.IModel