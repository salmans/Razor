{- This file is part of Razor.

  Razor is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Razor is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Razor.  If not, see <http://www.gnu.org/licenses/>.

  Module      : Common.Test.TestData
  Description : Test data for common data types
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

module Common.Test.TestData where

-- Standard
import qualified Data.Bimap as Bimap

-- Syntax
import Syntax.Term

-- Common
import Common.Observation
import Common.Provenance

-- Test
import Syntax.Test.TestData


obs_Pa        = Obs atm_Pa
obs_Complex4  = Obs atm_Complex4
obs_Complex5  = Obs atm_Complex5
obs_ElemE0    = Obs atm_ElemE0
obs_ElemE1    = Obs atm_ElemE1
obs_ElemE2    = Obs atm_ElemE2
obs_EqlE0E0   = Obs atm_EqlE0E0
obs_EqlE1E1   = Obs atm_EqlE1E1
obs_EqlE2E2   = Obs atm_EqlE2E2
obs_Pe0       = Obs atm_Pe0
obs_Pe1       = Obs atm_Pe1
obs_Qe0       = Obs atm_Qe0
obs_Qe1       = Obs atm_Qe1
obs_Re0       = Obs atm_Re0
obs_Re1       = Obs atm_Re1
obs_Se0       = Obs atm_Se0
obs_Se1       = Obs atm_Se1
obs_Ue0       = Obs atm_Ue0
obs_Ue1       = Obs atm_Ue1
obs_Qe0e0     = Obs atm_Qe0e0
obs_Qe0e1     = Obs atm_Qe0e1
obs_Re0e0     = Obs atm_Re0e0
obs_Re0e1     = Obs atm_Re0e1
obs_Re2e1     = Obs atm_Re2e1

obseq_Triv    = ObservationSequent [] []
obseq_Simple7 = ObservationSequent [] [[obs_Pa]]
obseq_Simple8 = ObservationSequent [obs_Pa] []


elmprov_prov1 = Bimap.fromList [ (elm "c1", consTerm "c1")
                               , (elm "c2", consTerm "c2")]
prov_prov1    = ProvInfo elmprov_prov1

elmprov_prov2 = Bimap.fromList [ (elm "c1", consTerm "c1")
                               , (elm "c2", consTerm "c2")
                               , (elm "e" , consTerm "a")]
prov_prov2    = ProvInfo elmprov_prov2

elmprov_prov3 = Bimap.fromList 
                [ (elm "c1", consTerm "c1")
                , (elm "c2", consTerm "c2")
                , (elm "e" , Fn "f" [consTerm "c1", consTerm "c2"])]
