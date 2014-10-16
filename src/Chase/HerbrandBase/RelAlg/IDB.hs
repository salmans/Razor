{- Razor
   Module      : Chase.HerbrandBase.RelAlg.IDB
   Description : Implements the database layer
   Maintainer  : Salman Saghafi -}

module Chase.HerbrandBase.RelAlg.IDB where

-- Standard
import Data.List as List ((\\), union)

-- Control
import Control.Applicative

-- RelAlg

{-| Set is a container for tuples of the database. In this implementation, sets
  are simply Haskell sets. 

 [@contents@] returns a set as a Haskell set -}
newtype Set t = Set {contents :: [t]} 
    deriving (Eq, Show)

{-| returns an empty 'Set' -}
emptySet :: Set t
emptySet = Set []

{-| Reads a 'Set' from a list -}
fromList :: Ord a => [a] -> Set a
fromList list = Set list


{-| Projecting a 'Set' of type @t1@ to a set of type @t2@ -}
newtype Project t1 t2 = Project (t1 -> t2)

{-| Projects a 'Set' of type @t1@ to a 'Set' of type @t2@ according to a 
 'Project' instance from @t1@ to @t2@. The function simply uses 'map' for sets 
 under the hood. -}
project :: Project t1 t2 -> Set t1 -> Set t2
project (Project f) (Set set) = 
    Set $ map f set
    -- Set $ ExSet.map f set

{-| A selection condition is a decision function over the elements of a 'Set' -}
newtype Select t = Select (t -> Bool)

{-| Selects records of a 'Set' according to a Select condition. -}
select :: Select t -> Set t -> Set t
select (Select f) (Set set) = 
    Set $ filter f set

{-| Joins two sets of types @t1@ and @t2@ according ot a join condition 
  provided by a 'Select' instance. -}
join :: Select (t1, t2) -> Set t1 -> Set t2 -> Set (t1, t2)
join sel (Set set1) (Set set2) = 
    select sel $ Set (prod set1 set2)
    where prod xs ys = [(x, y) | x <- xs, y <- ys]

-- Helper functions:
{-| Unions two sets. It is implied that the two sets contain unionable columns.
  -}
union :: Eq a => Set a -> Set a -> Set a
union (Set set1) (Set set2) = Set $ List.union set1 set2


{-| Returns the difference of the two sets. -}
difference :: Eq a => Set a -> Set a -> Set a
difference (Set set1) (Set set2) = Set $ set1 \\ set2