{- Razor
   Module      : Chase.HerbrandBase.RelAlg.IDB
   Description : Implements the database layer
   Maintainer  : Salman Saghafi -}

module Chase.HerbrandBase.RelAlg.IDB where

-- Standard
import Data.List as List ((\\), union)

-- Tools
import qualified Tools.ExtendedSet as ExSet

-- Control
import Control.Applicative

-- RelAlg

{-| Set is a container for tuples of the database. In this implementation, sets
  are simply Haskell sets. 

 [@contents@] returns a set as a Haskell set -}
newtype Set t = Set {contents :: ExSet.Set t} 
    deriving (Eq, Show)

{-| returns an empty 'Set' -}
emptySet :: Set t
emptySet = Set ExSet.empty

{-| Reads a 'Set' from a list -}
fromList :: Ord a => [a] -> Set a
fromList list = Set $ ExSet.fromList list


{-| Projecting a 'Set' of type @t1@ to a set of type @t2@ -}
newtype Project t1 t2 = Project (t1 -> t2)

{-| Projects a 'Set' of type @t1@ to a 'Set' of type @t2@ according to a 
 'Project' instance from @t1@ to @t2@. The function simply uses 'map' for sets 
 under the hood. -}
project :: (Ord t1, Ord t2) => Project t1 t2 -> Set t1 -> Set t2
project (Project f) (Set set) = 
    Set $ ExSet.fromList $ map f $ ExSet.toList set
    -- Set $ ExSet.map f set

{-| A selection condition is a decision function over the elements of a 'Set' -}
newtype Select t = Select (t -> Bool)

{-| Selects records of a 'Set' according to a Select condition. -}
select :: (Ord t) => Select t -> Set t -> Set t
select (Select f) (Set set) = 
    Set $ ExSet.fromList $ filter f $ ExSet.toList set
    -- Set $ ExSet.filter f set

{-| Joins two sets of types @t1@ and @t2@ according ot a join condition 
  provided by a 'Select' instance. -}
join :: (Ord t1, Ord t2) => Select (t1, t2) -> Set t1 -> Set t2 -> Set (t1, t2)
join sel (Set set1) (Set set2) = 
    let l1 = ExSet.toList set1
        l2 = ExSet.toList set2
    in  select sel $ Set $ ExSet.fromList (prod l1 l2)
    where prod xs ys = [(x, y) | x <- xs, y <- ys]
    -- select sel $ Set (ExSet.concatMap (\x -> 
    --                   ExSet.concatMap (\y -> ExSet.singleton (x,y)) set2) set1)

-- Helper functions:
{-| Unions two sets. It is implied that the two sets contain unionable columns.
  -}
union :: Ord a => Set a -> Set a -> Set a
union (Set set1) (Set set2) = 
    Set $ ExSet.fromList $ List.union (ExSet.toList set1) (ExSet.toList set2)
    -- Set $ ExSet.union s2 s1


{-| Returns the difference of the two sets. -}
difference :: Ord a => Set a -> Set a -> Set a
difference (Set set1) (Set set2) = 
    Set $ ExSet.fromList $ (ExSet.toList set1) \\ (ExSet.toList set2)
    -- Set $ ExSet.difference s1 s2