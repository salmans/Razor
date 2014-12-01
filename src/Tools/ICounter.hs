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

  Module      : Tools.ICounter
  Description : The module implements an interface for working with Counter,
  which is used to keep track of a counting value in a computation.
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

module Tools.ICounter where

-- Control
import qualified Control.Monad.State.Lazy as State

{-| Monad and monad transformer for counting. -}
type Counter  = State.State Int
type CounterT = State.StateT Int


{-| Increments the value in a 'Counter' and returns the current value of the 
  counter. -}
increment :: Counter Int
increment =  State.get >>= 
             (\c -> State.modify (+1) >>
             (return c))

{-| Increments the value in a 'CounterT' and returns the current value of the
  counter. -}
incrementT :: Monad m => CounterT m Int
incrementT = State.get >>= 
             (\c -> State.modify (+1) >>
             (return c))