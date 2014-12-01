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

  Module      : Tools.IUtils
  Description : Implements helper functions.
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

module Tools.IUtils where

-- Standard
import Data.Char (isSpace)
import Data.List (nub)

{-| Computes the union of a list of lists. -}
unions :: (Ord a) => [[a]] -> [a]
unions ls = nub $ foldr (++) [] ls

{-| Given an input string (corresponding to a line in the theory file), returns
  True if the line is not commented out and is not blank. -}
isRealLine :: String -> Bool
isRealLine l = 
    case l of
      [] -> False
      ('-':'-':_) -> False
      otherwise -> any (not.isSpace) l
