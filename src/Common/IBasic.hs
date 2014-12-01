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

  Module      : Common.IBasic
  Description : Internal implementation for basic data types.
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

module Common.IBasic where

-- Control
import qualified Control.Monad.State.Lazy as State (get, put)

-- Tools
import Tools.Counter (Counter)

{-| Id is the type of (unique) identifiers for various instances of type. -}
type Id  = Int

{-| Symbols -}
type Sym = String

{-| Returns a fresh symbol with the prefix given. -}
freshSymbol :: Sym -> Counter Sym
freshSymbol sym = State.get >>=
                  (\c -> State.put (c + 1) >>
                  (return $ sym ++ (show c)))
--                   (return $ sym ++ "#" ++ (show c))) -- FIXME: SBV complains