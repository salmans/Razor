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

  Module      : Tools.ITrace
  Description : Implements helper functions for tracing.
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

module Tools.ITrace where

import Debug.Trace

traceList :: (Show a) => [a] -> b -> b
traceList itms x = foldr traceShow x itms

traceListIf :: (Show a) => Bool -> [a] -> b -> b
traceListIf cond itms x = 
    if cond 
    then traceList itms x 
    else x

traceEval :: Show a => a -> a
traceEval x =  traceShow x x

traceEvalWith :: Show b => (a -> b) -> a -> a
traceEvalWith f x = traceShow (f x) x

traceIf :: Show a => Bool -> a -> b -> b
traceIf cond x y = if cond then (traceShow x) y else y

traceStringList :: [String] -> b -> b
traceStringList itms x = foldr trace x itms

traceStringListIf :: Bool -> [String] -> b -> b
traceStringListIf cond itms x = if cond 
                                then traceStringList itms x 
                                else x
