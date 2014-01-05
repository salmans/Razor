module Chase.Problem.Provenance where

import qualified Data.Map as Map
import Control.Applicative

import Utils.GeoUtilities
import Chase.Problem.Observation
import Chase.Problem.BaseTypes

{-| A provenance is either a pair of (id, sub) where id is an identifier for 
  the sequent for which a chase step added the fact, and sub is the 
  substitution that constructed the observation, or a decision made by the user.
 -}
data Prov = ChaseProv ProvTag ID Sub
          | UserProv
  deriving Show

{-| Provenance information for models maps every observation to a provenance 
  list. -}
data ProvInfo = ProvInfo { provInfoData     :: Map.Map Obs [Prov]
                         , provInfoLastTag  :: ProvTag }
                deriving Show