module Chase.Problem.BaseTypes where

import Utils.GeoUtilities

{-| Unique identifier -}
type ID = Int

{-| A provenance is a pair of (id, sub) where id is an identifier for the 
  sequent for which a chase step added the fact, and sub is the substitution 
  that constructed the observation.-}
type Prov = (ID, Sub)
