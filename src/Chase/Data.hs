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

  Module      : Chase.Data
  Description : Chase.Data provides the basic data-structures that are used by
  the Chase algorithm.
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

module Chase.Data ( PossibleFacts (..) , ChaseImpl (..)
                  , SequentMap, buildSequentMap
                  , Problem (..), buildProblem
                  , problemSequentMap, problemBase, problemDelta, problemProvs
                  , problemSATIterator
                  , PushM, runPushM
                  , liftPushMBase, liftPushMProvs, liftPushMCounter
                  , liftPushMSATIterator, liftPushMConfig
                  , PullM, runPullM, evalPullM, liftPullMBase, liftPullMProvs
                  , ChaseM (..), liftChaseMState, liftChaseMCounter
                  , liftChaseMConfig
                  , incompleteSequent
                  , replaceIncompleteFn, replaceIncompleteEx
                  , replaceRelaxIncompleteEx ) where

import Chase.IData