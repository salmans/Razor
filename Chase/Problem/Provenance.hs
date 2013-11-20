module Chase.Problem.Provenance where

import qualified Data.Map as Map

import Utils.GeoUtilities

import Chase.Problem.Observation

{-| Unique identifier -}
type ID = Int

{-| A provenance is a pair of (id, sub) where id is an identifier for the 
  sequent for which a chase step added the fact, and sub is the substitution 
  that constructed the observation.-}
type Prov = (ID, Sub)

{-| Provenance information for models maps every observation to a provenance 
  list. -}
type ProvInfo = Map.Map Obs [Prov]