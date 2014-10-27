{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- Razor
   Module      : Chase.HerbrandBase.RelAlg.ILang
   Description : Implements the language of relational algebra
   Maintainer  : Salman Saghafi -}

module Chase.HerbrandBase.RelAlg.ILang where

-- Standard
import Data.List (union, nub)
import qualified Data.Vector as Vect
import Data.Vector ((!))
import qualified Data.Map as Map

-- Control
import Control.Applicative

-- Syntax
import Syntax.Term ( Variable, Constant (..), Element (..), FnSym
                   , ExistsSub, emptyExistsSub)
import Syntax.Geometric (RelSym)

-- RelAlg
import qualified Chase.HerbrandBase.RelAlg.DB as DB


{-| An atrribute is a variable name. -}
type Attribute  = Variable

{-| A column number -}
type Column     = Int

{-| A Header is a map from 'Attribute's to 'Column's (representing a 
  column number in a 'Table'). -}
type Header = Map.Map Attribute Column

{-| TableRef captures various types of tables in HerbrandBase. -}
data TableRef  = ConstTable Constant -- constant tables
               | RelTable   RelSym   -- relation tables
               | FnTable    FnSym    -- function tables
               | DomTable            -- domain table
                 deriving (Eq, Ord, Show)

{- A vector of elements. -}
type Tup       = Vect.Vector Element
{-| A TupleD is a vector of elements decorated with an object of type @a@. -}
data TupleD a  = Tuple { tupleElems :: Tup 
                       , tupleDec   :: a
                       } deriving (Show, Eq, Ord)

{-| Tuple is nothing more than a vector of elements. -}
type Tuple     = TupleD ()

{-| TuplePair is a Tuple decorated with another 'Tuple'. -}
type TuplePair = TupleD Tup

{-| TupleSub is a Tuple decorated with 'ExistSub'. This kind of tuple is used 
  when evaluating a sequent in the database since it also returns the 
  substitutions that are needed when computing observational sequents that are 
  fed to the SAT/SMT solver. -}
type TupleSub  = TupleD ExistsSub

{-| Puts a vector of elements inside a tuple. -}
tuple :: Tup -> Tuple
tuple t = Tuple t ()

{-| Forgets the decoration of a 'Tuple' -}
undecorate :: TupleD a -> Tuple
undecorate (Tuple tup _) = Tuple tup ()

{-| Reads a 'Tuple' from a list. -}
tupleFromList :: [Element] -> Tuple
tupleFromList = tuple . Vect.fromList

{-| Reads a 'TuplePair' from a list. -}
tuplePairFromList :: ([Element], [Element]) -> TuplePair
tuplePairFromList (es1, es2) = Tuple (Vect.fromList es1) (Vect.fromList es2)

{-| TableD is a datastructure that stores the tuples of a relation, constant, or
  function in a database. The tuples may be decorated. -}
type TableD a = DB.Set (TupleD a)

{-| Table is a 'TableD' of undecorated tuples. -}
type Table    = TableD ()

{-| 'TablePair' is a set of tuples decorated by another vector of elements. -}
type TablePair = TableD Tup

{-| 'TableSub' is a set of tuples decorated by 'ExistsSub'. -}
type TableSub  = TableD ExistsSub

{-| Reads a the records of a 'Table' from a list. -}
recordsFromList :: [Tuple] -> Table
recordsFromList = DB.fromList

{-| Reads a the records of a 'TablePair' from a list. -}
recordPairsFromList :: [TuplePair] -> TablePair
recordPairsFromList = DB.fromList

{-| Reads a the tuples of a 'Table' from a list of lists. -}
tableFromList :: [[Element]] -> Table
tableFromList = recordsFromList.(tupleFromList <$>)

{-| Reads a the tuples of a 'TablePair' from a list of lists. -}
tablePairFromList :: [([Element], [Element])] -> TablePair
tablePairFromList = recordPairsFromList.(tuplePairFromList <$>)

{-| Empty 'TableD' -}
emptyTableD :: TableD a
emptyTableD =  DB.empty

{-| Empty 'Table' -}
emptyTable :: Table
emptyTable  =  emptyTableD

{-| Empty 'TablePair' -}
emptyTablePair :: TablePair
emptyTablePair =  emptyTableD

{-| Empty 'TableSub' -}
emptyTableSub :: TableSub
emptyTableSub =  emptyTableD

{-| Returns true if the input 'Table' is empty. -}
nullTable :: Table -> Bool
nullTable =  DB.null

{-| Returns true if the input 'TableD' is empty. -}
nullTableD :: TableD a -> Bool
nullTableD =  DB.null

{-| Returns true if the input 'TablePair' is empty. -}
nullTablePair :: TablePair -> Bool
nullTablePair =  nullTableD

{-| Returns true if the input 'TablePair' is empty. -}
nullTableSub :: TableSub -> Bool
nullTableSub =  nullTableD

{-| Full 'TableD': the input parameter is used for the decorating data. -}
fullTableD :: Ord a => a -> TableD a
fullTableD =  \x -> DB.fromList [Tuple Vect.empty x]

{-| Just as a contract, let's use a table with a single element @True@ to denote
   a view for a sequent's empty body -}
fullTable :: Table
fullTable =  fullTableD ()

{-| Full 'TableSub' -}
fullTableSub :: TableSub
fullTableSub =  fullTableD emptyExistsSub


{-| Just as a contract, @[Nothing]@ is the schema of 'fullTable'. -}
fullTableHeader :: Header
fullTableHeader =  Map.empty

{-| Decorates a 'Table' with the information of type @a@ -}
decorateTable :: Ord a => Table -> a -> TableD a
decorateTable tbl info = DB.map (\(Tuple t _) -> Tuple t info) tbl

{-| Returns the data in an 'TableD' as a 'Table' -}
undecorateTable :: Ord a => TableD a -> Table
undecorateTable tbl = DB.map (\(Tuple t _) -> Tuple t ()) tbl

{-| A 'Database' represents a database of tables, mapping 'TableRef' to 'Table'.
 -}
type Database = Map.Map TableRef Table

{-| Empty 'Database' -}
emptyDatabase :: Database
emptyDatabase =  Map.empty


{-| Empty 'Database' inititialized with a set of 'Constant's. -}
emptyDatabaseWithConstants :: [Constant] -> Database
emptyDatabaseWithConstants []     = emptyDatabase
emptyDatabaseWithConstants consts =
    let elemTbl   = DB.fromList $ tuple.(Vect.singleton) <$> elems
        constTbls = [ (ConstTable c, mkCTbl name)| c@(Constant name) <- consts]
    in  Map.fromList $ (RelTable "@Element", elemTbl):constTbls
    where elems     = [ Element c | Constant c <- consts ]          
          mkCTbl    = DB.singleton . tuple . Vect.singleton . Element

{-| Returns true if the input 'Database' is empty. -}
nullDatabase :: Database -> Bool
nullDatabase =  Map.null

databaseFromList :: [(TableRef, Table)] -> Database
databaseFromList pairs = Map.fromList pairs
    

{-| RelExp defines a relational algebra expression for evaluating sequents in 
 HerbrandBase.

 [@TblEmpty@] An empty table, representing the empty head of a sequent
 [@TblFull@] A full table, representing the empty body of a sequent 
 [@Tbl@] A relational table
     1. [@table@] is a reference to a 'Table' referenced by the expression
     2. [@varPairs@] is a list of ('Column', 'Column') that maps the 
     columns of the table to columns of the (projected) target table where
     constant columns are projected out.
     3. [@header@] is the attribute 'Header' for this expression.
 [@Proj@] Projection of a relational expression. 
     1. [@expression@] is the relational expression being projected
     2. [@column@] is a the 'Column's that is projected out. It is important
     to notice that because every @Proj@ expression corresponds to an 
     existentially quantified formula, which in the current implementation has
     only one existentially quantified variable, the number of columns that are
     projected out is exactly one.
     3. [@header@] is the attribute 'Header' for this expression.
     4. [@skolemFn@] is the Skolem function associated to the existential
     quantifier that has been translated to the projection.
     5. [@unique@] provides a reference to a table (if a Just value) for which 
     the projected column should be unique. This is primarily used to preserve 
     the uniqueness of functional outputs.
 [@Sel@] Select from a relational expression. The parameters are:
     1. [@expression@] is the expression on which selection is applied.
     2. [@columnPairs@] is a list of 'Column' pairs, mapping a columns previous
     position to its new position in the selected table.
     3. [@header@] is the attribute 'Header' for this expression.
 [@Join@] Join of two relational expressions.
     1. [@leftExpression@] is the left 'RelExp' in the join expression.
     2. [@rightExpression@] is the right 'RelExp' in the join expression.
     3. [@header@] is the attribute 'Header' for this expression.
     4. [@joinTransformer@] Maintains a transforming function to reorder the 
     columns of the two tables to match the header that is advertised by the 
     join expression.

 [@Delta@] Changes in a relational expression: this usually denotes evaluating
 the expression in a database that contains the changes of a primary database.
     1. [@expression@] is the expression for which the changes is being computed.
     2. [@header@] is the attribute 'Header' for this expression.
 [@Union@] represents a union of three components of a diffrential formula 
 corresponding to a join expression: @P |X| Q = P |X| dQ + dQ |X| dP + dP |X| Q@
     1. [@leftExpression@] is the expression on left (@P@)
     2. [@leftDelta@] is the changes in the left expression (Delta @P@)
     3. [@rightExpression@] is the expression on right (@Q@)
     4. [@rightDelta@] is the changes in the right expression (Delta @Q@)
     5. [@header@] is the attribute 'Header' for this expression.
     6. [@joinTransformer@] serves the same purpose as joinTransformer function
     for 'Join'.
 -}
data RelExp = TblEmpty
            | TblFull
            | Tbl   { table            :: TableRef
                    , varPairs         :: [(Column, Column)]
                    , header           :: Header
                    }
            | Proj  { expression       :: RelExp 
                    , column           :: Column
                    , header           :: Header
                    , skolemFn         :: FnSym
                    , unique           :: Maybe RelExp
                    }
            | Sel   { expression       :: RelExp 
                    , columnPairs      :: [(Column, Column)] 
                    , header           :: Header
                    }
            | Join  { leftExpression   :: RelExp 
                    , rightExpression  :: RelExp                                           
                    , header           :: Header
                    , joinTransformer  :: TupleSub -> TupleSub -> TupleSub
                    }
            | Delta { expression       :: RelExp 
                    , header           :: Header
                    }
            | Union { leftExpression   :: RelExp 
                    , leftDelta        :: RelExp 
                    , rightExpression  :: RelExp
                    , rightDelta       :: RelExp
                    , header           :: Header
                    , joinTransformer  :: TupleSub -> TupleSub -> TupleSub
                    }

{- Eq instance for 'RelExp' -}
-- NOTE: this instantiation is naive; its performance may be improved
instance Eq RelExp where
    TblEmpty == TblEmpty                                = True
    TblFull == TblFull                                  = True
    Tbl r cs h == Tbl r' cs' h'                         = 
        r == r' && cs == cs' && h == h'
    Proj e c h f u == Proj e' c' h' f' u'               = 
        e == e' && c == c' && h == h' && f == f' && u == u'
    Sel e cs h == Sel e' cs' h' =
        e == e' && cs == cs' && h == h'
    Join l r h _ == Join l' r' h' _                     =
        l == l' && r == r' && h == h'
    Delta e h == Delta e' h'                            =
        e == e' && h == h'
    Union le ld re rd h _ == Union le' ld' re' rd' h' _ =
        le == le' && ld == ld' && re == re' && rd == rd' && h == h'
    exp == exp'                                         = False --otherwise

{- Show instance for 'RelExp' -}
instance Show RelExp where
    show _ = ""

--------------------------------------------------------------------------------
-- Some Helpers

{-| Creates an instance of 'DB.Project' for removing the 'Column' at a given 
  index. The function also accepts an optional function to update the decorating
  info of the tuple. 
 -}
deleteColumnProjector :: Column -> Maybe (TupleD a -> a)
                      -> DB.Project (TupleD a) (TupleD a)
deleteColumnProjector col updateFn = 
    -- del is the map function that eliminates the projected column:
    let del t@(Tuple v d) i = 
            let (hd, tl)  = Vect.splitAt i v
                v'        = hd Vect.++ (Vect.tail tl) -- the new tuple
            in case updateFn of
                 Nothing -> Tuple v' d
                 Just f  -> Tuple v' (f t) -- apply the update function on
                                           -- the decorating info 
    in DB.Project (\tup -> del tup col)

{-| Given a list of 'Column' pairs, returns a 'DB.Select' for selecting tuples
  of a 'Table' in which elements at each pair of columns are equal -}
similarColumnsSelector :: [(Column, Column)] -> DB.Select (TupleD a)
similarColumnsSelector colPairs = 
    DB.Select $ \(Tuple x _) -> 
        and $ (\(p1, p2) -> x Vect.! p1 == x Vect.! p2) <$> colPairs

{-| Given a list of 'Column' pairs, returns a 'DB.Select' for selecting tuples
  of a 'Table' in which elements at each pair of columns are equal -}
columnValuesSelector :: [(Column, Element)] -> DB.Select (TupleD a)
columnValuesSelector colPairs = 
    DB.Select $ \(Tuple x _) -> 
        and $ (\(p1, elm) -> x Vect.! p1 == elm) <$> colPairs

{-| Merges a pair of 'Set' instances into one 'Set'. This kind of pairs are 
   constructed by the join operation in "Chase.HerbrandBase.RelAlg.DB".
   The function also removes duplicate columns based on a list of 'Column's
   from the second table. Moreover, it merges the decorating data of the 
   corresponding tuples using the given function. -}
mergeJoinTableDs :: (Ord a) => DB.Set (TupleD a, TupleD a) -> [Int] -> 
                    (a -> a -> a) -> TableD a
mergeJoinTableDs tbl cols fun =
    DB.map mergeFunc tbl
    where mergeFunc = \(Tuple a x , Tuple b y) -> 
                      Tuple (a Vect.++ (del b cols)) (fun x y)
          del v is  = let ps  = Vect.zip v 
                                $ Vect.fromList [0..(Vect.length v - 1)]
                      in  fst <$> Vect.filter ((flip notElem is).snd) ps

mergeJoinTables :: DB.Set (Tuple, Tuple) -> [Int] -> Table
mergeJoinTables =  \x y -> mergeJoinTableDs x y const

{-| Given an input 'RelExp', returns all 'TableRef's of the tables that are
  addressed by the input relational expression.
 -}
relExpRefs :: RelExp -> [TableRef]
relExpRefs TblEmpty                   = []
relExpRefs TblFull                    = []
relExpRefs (Tbl   ref  _ _        )   = [ref]
relExpRefs (Join  lExp rExp _ _     ) = 
    (relExpRefs lExp) `union` (relExpRefs rExp)
relExpRefs (Union lExp rExp _ _ _ _ ) = 
    (relExpRefs lExp) `union` (relExpRefs rExp)
relExpRefs exp                        = relExpRefs (expression exp)
    -- Taking advantage of the fact that all remaining data constructors have
    -- an "expression" field selector.

{-| Returns true if any of the input 'TableRef's point to any 'Table' in the
 input 'Database'. -}
refsInDatabase :: [TableRef] -> Database -> Bool
refsInDatabase refs db = or $ ((flip Map.member) db) <$> refs


{-| Removes duplicate tuples of the input 'TableD' -}
nubTable :: Eq a => TableD a -> TableD a
nubTable =  DB.nub

{-| The union of two 'TableD's. The function assumes that the two tables are 
  unionable. -}
unionTables :: (Ord a) => TableD a -> TableD a -> TableD a
unionTables = DB.union

{-| The union of list of 'TableD's. The function assumes that the tables are 
  unionable. -}
unionsTables :: (Ord a) => [TableD a] -> TableD a
unionsTables = DB.unions

{-| The union of two 'Database's -}
unionDatabases :: Database -> Database -> Database
unionDatabases  db1 db2 = Map.unionWith (unionTables) db1 db2

{-| Assuming that the input 'Table's are unionable, returns the tuples in the
   first input that is not contained in the second input. -}
diffTables :: Table -> Table -> Table
diffTables =  DB.difference

{-| Returns the difference of two input databases. For the tables that are in 
   common, the function removes the table (and its reference) in the output
   databases. -}
diffDatabases :: Database -> Database -> Database
diffDatabases db1 db2 = Map.differenceWith diffFunc db1 db2
    where diffFunc = \t1 t2 -> let diff = diffTables t1 t2
                               in  if   nullTable diff 
                                   then Nothing
                                   else Just diff

{-| Removes references to empty tables from the input 'Database'. -}
removeEmptyTables :: Database -> Database
removeEmptyTables =  Map.filter (emptyTable /= )

{-| Returns the size of a database. -}
databaseSize :: Database -> Int
databaseSize db = Map.foldr (\set s -> s + (DB.size set)) 0 db