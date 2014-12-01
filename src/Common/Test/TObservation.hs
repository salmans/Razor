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

  Module      : Common.Test.TObservation
  Description : Unit tests for Observation
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

module Common.Test.TObservation where

-- HUnit
import Test.HUnit
import Test.HUnit.Text (runTestTT)

-- Syntax
import Syntax.Geometric (parseSequent)

-- Test
import Syntax.Test.TestData
import Common.Test.TestData

-- Common
import Common.IObservation
import Common.Data (SequentLike (..))

test_toObservation =
    [ "test toObservation for simple atom" ~:
      (Just obs_Pa) ~=? toObservation atm_Pa
    -- these should cause errors:
    -- , "test toObservation for non-closed atom" ~:
    --   obs_Pc ~=? toObservation atm_Px
    -- , "test toObservation for non-closed atom" ~:
    --   obs_Pc ~=? toObservation atm_Complex2

    , "test toObservation for complex atom" ~:
      (Just obs_Complex4) ~=? toObservation atm_Complex4
    , "test toObservation for complex atom" ~:
      (Just obs_Complex5) ~=? toObservation atm_Complex5
    ]

-- test_fromSequentForObservationSequent =
--     [ "test fromSequentForObservationSequent for trivial sequent" ~:
--       obseq_Triv ~=? fromSequent seq_Triv
--     , "test fromSequentForObservationSequent for sequent with empty body" ~:
--       obseq_Simple7 ~=? fromSequent seq_Simple7
--     , "test fromSequentForObservationSequent for sequent with empty head" ~:
--       obseq_Simple8 ~=? fromSequent seq_Simple8
--     -- , "test fromSequentForObservationSequent for conjunction in head" ~:
--     --   obseq_Conjunction1 ~=? 
--     ]

test_all = [] -- test_toObservation -- ++ test_fromSequentForObservationSequent
