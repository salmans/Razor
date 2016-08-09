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

  Module      : Chase.PossibleFacts.RelAlg.IListDB
  Description : Implements the database layer based on Haskell lists
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

module Chase.PossibleFacts.RelAlg.ListDB where

-- Standard
import Prelude hiding (map, filter)
import qualified Data.List as List (null, elem, (\\), union, nub,  map, filter)

-- Control
import Control.Applicative

{-| Set is a container for tuples of the database. In this implementation, sets
  are simply Haskell sets. 

 [@contents@] returns a set as a Haskell set -}
newtype Set t = Set {contents :: [t]} 
    deriving (Eq, Show)

{-| returns an empty 'Set' -}
empty :: Set t
empty = Set []

{-| Creates a singleton 'Set' from the input element. -}
singleton :: t -> Set t
singleton x = Set [x]

{-| Returns a member of the input 'Set'. This function is often used on 
  functional tables where a unique record in the set is expected. -}
oneMember :: Set t -> t
oneMember (Set set) = head set

{-| Inserts a new element to the set. -}
insert :: t -> Set t -> Set t
insert x (Set set) = Set $ x:set

{-| Returns true if the input 'Set' is empty -}
null :: Set t -> Bool
null (Set set) = List.null set

{-| Returns True if the given element is a member of the given 'Set'. -}
elem :: Eq t => t -> Set t -> Bool
elem x (Set set) = x `List.elem` set

{-| Reads a 'Set' from a list -}
fromList :: Ord a => [a] -> Set a
fromList list = Set list

{-| Returns a 'Set' as a list -}
toList :: Ord t => Set t -> [t]
toList (Set set) = set

{-| Mapping sets. -}
map :: (t1 -> t2) -> Set t1 -> Set t2
map f (Set set) = Set $ List.map f set

{-| Filtering sets. -}
filter :: (t -> Bool) -> Set t -> Set t
filter f (Set set) = Set $ List.filter f set

{-| Removes duplicate elements of the 'Set' -}
nub :: Eq t => Set t -> Set t
nub (Set set) = Set $ List.nub set

{-| Returns the size of a 'Set' -}
size :: Set t -> Int
size (Set set) = length set

{-| Projecting a 'Set' of type @t1@ to a set of type @t2@ -}
newtype Project t1 t2 = Project (t1 -> t2)

{-| Projects a 'Set' of type @t1@ to a 'Set' of type @t2@ according to a 
 'Project' instance from @t1@ to @t2@. The function simply uses 'map' for sets 
 under the hood. -}
project :: Project t1 t2 -> Set t1 -> Set t2
project (Project f) set = map f set

{-| A selection condition is a decision function over the elements of a 'Set' -}
newtype Select t = Select (t -> Bool)

{-| Selects records of a 'Set' according to a Select condition. -}
select :: Select t -> Set t -> Set t
select (Select f) set = filter f set

{-| Joins two sets of types @t1@ and @t2@ according ot a join condition 
  provided by a 'Select' instance. -}
join :: Select (t1, t2) -> Set t1 -> Set t2 -> Set (t1, t2)
join sel (Set set1) (Set set2) = 
    select sel $ Set (prod set1 set2)
    where prod xs ys = [(x, y) | x <- xs, y <- ys]

-- Helper functions:
{-| Unions two sets. It is assumed that the two sets are unionable. -}
union :: Eq a => Set a -> Set a -> Set a
union (Set set1) (Set set2) = Set $ List.union set1 set2

{-| Unions two sets. It is assumed that sets are unionable. -}
unions :: Eq a => [Set a] -> Set a
unions sets = let ss = List.map (\(Set s) -> s) sets
              in  Set $ foldl List.union [] ss

{-| Returns the difference of the two sets. -}
difference :: Eq a => Set a -> Set a -> Set a
difference (Set set1) (Set set2) = Set $ set1 List.\\ set2
