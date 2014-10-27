{- Razor
   Module      : Chase.HerbrandBase.RelAlg.IDB
   Description : Implements the database layer based on Haskell sets
   Maintainer  : Salman Saghafi -}

module Chase.HerbrandBase.RelAlg.ISetDB where

-- Standard
import Prelude hiding (map, filter)
import qualified Data.List as List (map)

-- Tools
import qualified Tools.ExtendedSet as ES


{-| Set is a container for tuples of the database. In this implementation, sets
  are simply Haskell sets. 

 [@contents@] returns a set as a Haskell set -}
newtype Set t = Set {contents :: ES.Set t} 
    deriving (Eq, Show)

{-| Returns an empty 'Set' -}
empty :: Set t
empty = Set ES.empty

{-| Creates a singleton 'Set' from the input element. -}
singleton :: t -> Set t
singleton x = Set $ ES.singleton x

{-| Returns a member of the input 'Set'. This function is often used on 
  functional tables where a unique record in the set is expected. -}
oneMember :: Set t -> t
oneMember (Set set) = ES.findMin set

{-| Returns true if the input 'Set' is empty -}
null :: Set t -> Bool
null (Set set) = ES.null set

{-| Returns True if the given element is a member of the given 'Set'. -}
elem :: (Ord t) => t -> Set t -> Bool
elem x (Set set) = x `ES.member` set

{-| Reads a 'Set' from a list -}
fromList :: Ord t => [t] -> Set t
fromList list = Set $ ES.fromList list

{-| Returns a 'Set' as a list -}
toList :: Ord t => Set t -> [t]
toList (Set set) = ES.toList set

{-| Mapping sets. -}
map :: (Ord t1, Ord t2) => (t1 -> t2) -> Set t1 -> Set t2
map f (Set set) = Set $ ES.map f set

{-| Filtering sets. -}
filter :: (t -> Bool) -> Set t -> Set t
filter f (Set set) = Set $ ES.filter f set

{-| Removes duplicate elements of the 'Set' -}
nub :: Set t -> Set t
nub = id

{-| Returns the size of a 'Set' -}
size :: Set t -> Int
size (Set set) = ES.size set

{-| Projecting a 'Set' of type @t1@ to a set of type @t2@ -}
newtype Project t1 t2 = Project (t1 -> t2)

{-| Projects a 'Set' of type @t1@ to a 'Set' of type @t2@ according to a 
 'Project' instance from @t1@ to @t2@. The function simply uses 'map' for sets 
 under the hood. -}
project :: (Ord t1, Ord t2) => Project t1 t2 -> Set t1 -> Set t2
project (Project f) set = map f set

{-| A selection condition is a decision function over the elements of a 'Set' -}
newtype Select t = Select (t -> Bool)

{-| Selects records of a 'Set' according to a Select condition. -}
select :: Select t -> Set t -> Set t
select (Select f) set = filter f set

{-| Joins two sets of types @t1@ and @t2@ according ot a join condition 
  provided by a 'Select' instance. -}
join :: (Ord t1, Ord t2) => Select (t1, t2) -> Set t1 -> Set t2 -> Set (t1, t2)
join sel (Set set1) (Set set2) = 
    let list1 = ES.toList set1
        list2 = ES.toList set2
    in  select sel $ Set $ ES.fromList (prod list1 list2)
    where prod xs ys = [(x, y) | x <- xs, y <- ys]

-- Helper functions:
{-| Unions two sets. It is assumed that the two sets are unionable.-}
union :: Ord a => Set a -> Set a -> Set a
union (Set set1) (Set set2) = Set $ ES.union set1 set2

{-| Unions a list of sets. It is assumed that the sets are unionable. -}
unions :: Ord a => [Set a] -> Set a
unions sets = let ss = List.map (\(Set s) -> s) sets
              in  Set $ ES.unions ss


{-| Returns the difference of the two sets. -}
difference :: Ord a => Set a -> Set a -> Set a
difference (Set set1) (Set set2) = Set $ ES.difference set1 set2