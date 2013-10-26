module Chase.Problem.RelAlg.IRelAlg where


-- General Modules
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Control.Applicative

-- Logic Modules
import Formula.SyntaxGeo
import Utils.GeoUtilities

-- Chase Modules:
import Chase.Problem.Observation
import qualified RelAlg.DB as DB

-- Other Modules
import Debug.Trace


{- Data Types -}

{- A record in a table in models. -}
type Record = [Term]

{- A table in models. -}
type Table = DB.Set Record

{- Different types of tables: -}
data TableRef  = RelTable Sym -- A table for a relation
               | FunTable Sym -- A table for a function
               | DomTable -- A table containing elements of the domain
                 deriving (Eq, Ord, Show)
    

{- A map from relational symbols to tables -}
type Tables = Map.Map TableRef Table

{-| empty tables (corresponding to an empty model) -}
emptyTables = Map.singleton DomTable (DB.Set [])


{- Relational expression, corresponding to a view for a geometric formula
   (without disjunction) -}
data RelExp = TblEmpty
            | TblFull
            | Tbl  TableRef
            | Proj RelExp [Int]
            | Slct RelExp [(Int, Int)] [(Int, Term)]
            | Join RelExp RelExp [(Int, Int)]
            deriving (Show, Eq)

{- An equation is a pair of terms -}
data Equation = Equ Term Term
              deriving (Show, Eq)

-- THIS HAS TO BE MOVED SOMEWHERE ELSE!
instance Functor DB.Set where
    fmap f (DB.Set x) = DB.Set (map f x)

instance Applicative DB.Set where
    pure x                = DB.Set (pure x)
    DB.Set f <*> DB.Set x = DB.Set (f <*> x)

{- A column label is either a variable or Nothing -}
type Label  = Maybe Var
type Labels = [Maybe Var]
--------------------------------------------------------------------------------
{-| Creates a relational expression together with a list of attributes 
  (corresponding to (free and bound) variables in the formula) for a formula on 
  right of a sequent. Unlke the formulas on left, the formulas on right may have 
  disjunctions at their topmost level. -}
headRelExp :: Formula -> [(RelExp, Labels)]
headRelExp (Or fmla1 fmla2) = 
    union (headRelExp fmla1) (headRelExp fmla2)
headRelExp fmla             = [relExp fmla]

{-| Creates a relational expression and a list of variable labels for a formula 
  on left of a sequent. -}
bodyRelExp :: Formula -> (RelExp, Labels)
bodyRelExp = relExp

{- Given a disjunctions-free geometric formula, returns a relational expression 
 and a set of labels for variables as the attributes of the formula. -}
relExp :: Formula -> (RelExp, Labels)    
relExp fmla =
    (projectVars exp lbls, newLbls)
    where (exp, lbls) = formulaRelExp fmla
          newLbls     = nub $ filter (/= Nothing) lbls

{- Given a relational expression and its corresponding set of labels, returns 
 a new relational expression in which (1) dupliate variable labels and (2) 
 Nothing labels, are projected out. -}
projectVars :: RelExp -> Labels -> RelExp
projectVars exp lbls = 
    Proj exp pos
    where lblMap = createLabelMap lbls
          pos    = [fromJust p | l <- nub lbls, 
                    isJust l, 
                    let p = Map.lookup (fromJust l) lblMap, 
                    isJust p]

{- Builds a relational expression for a goemetric formula (disjunction free). 
 It also returns a set of variables that appear in every formula. -}
formulaRelExp :: Formula -> (RelExp, Labels)
formulaRelExp Fls                 = (TblEmpty, [])
formulaRelExp Tru                 = (TblFull , [Nothing])
formulaRelExp (Atm a)             = atomRelExp a
formulaRelExp (And fmla1 fmla2)   =
    if (fmla1 == Fls || fmla2 == Fls) 
    then (TblEmpty, []) -- shortcut Falsehood
    else joinRelExp res1 res2 -- Join
    where res1 = formulaRelExp fmla1  -- apply recursion
          res2 = formulaRelExp fmla2
formulaRelExp (Exists x fmla)     = (exp, newLbls)
    where (exp, lbls) = formulaRelExp fmla
          newLbls     = (\l -> if l == Just x then Nothing else l) <$> lbls

{- Builds a relational expression and a set of variable labels for a given 
   atomic formula. -}
atomRelExp :: Atom -> (RelExp, Labels)
atomRelExp a@(R "=" [t1, t2]) =
    let res1 = termRelExp t1
        res2 = termRelExp t2
    in  equalityRelExp res1 res2
atomRelExp a@(R sym ts)       = 
    let exp = if vPairs == [] && ePairs == []
              then Tbl (RelTable sym)
              else Slct (Tbl (RelTable sym)) vPairs ePairs
    in  foldl foldFunc (exp, lbls) $ zip ts [0..]
                  -- Select the records that match the terms of the relation
    where ePairs = elmPreds ts 
                 -- (1) Predicates that choose records whose entries match the 
                 -- elements in the atomic formula within the positions they 
                 -- appear.
          vPairs = varPreds ts
                 -- (2) predicates that force positions equal content in 
                 -- columns whose corresponding variables are the same in the 
                 -- atomic formula.
          lbls   = termLabels ts
          foldFunc (exp, ls) (term, i)  -- only care about variable labels
                 = case term of
                     Var v     -> (exp, ls)
                     -- dealt with in varPreds
                     Elm _     -> (exp, ls)
                     -- dealt with in elmPreds
                     Fn c []   -> (Join exp (Tbl (FunTable c)) [(i, 0)]
                                  , ls ++ [Nothing])
                     -- nothing left but constants
                     otherwise -> error $ "Chase.Problem.IRelAlg.atomRelExp: " ++
                                  "function symbols are not permitted!"

{- Creates a join expression for two input relational expressions and their
   corresponding variable labels. -}
joinRelExp :: (RelExp, Labels) -> (RelExp, Labels) -> (RelExp, Labels)
joinRelExp (exp1, lbls1) (exp2, lbls2) = 
    (Join exp1 exp2 pairs, lbls1 ++ lbls2)
    where pairs = varJoinPairs lbls1 lbls2
          -- Construct a set of pairs, representing a joining predicate.

{- Creates a join expression for two input relational expressions and their
   corresponding variable labels. -}
-- Salman: simplify considering functions are not permitted.
equalityRelExp :: (RelExp, Labels) -> (RelExp, Labels) -> (RelExp, Labels)
equalityRelExp (exp1, lbls1) (exp2, lbls2) = 
    (Join exp1 exp2 [pair], lbls1 ++ lbls2)
    where pair = (length lbls1 - 1, length lbls2 - 1)
          -- Construct a pair for joining the two sets.

{- Creates a relational expression and a set of labels for a given term -}
termRelExp :: Term -> (RelExp, Labels)
termRelExp (Var v)   = (Tbl DomTable, [Just v])
termRelExp (Fn c []) = (Tbl (FunTable c), [Nothing])
termRelExp _         = error $ "Chase.Problem.IRelAlg.termRelExp: function " ++
                       "symbols are not permitted!"

{- Constructs a set of pairs to filter the records of a set based on the 
elements appearing in the terms of an atomic formula. -}
elmPreds :: [Term] -> [(Int, Term)]
elmPreds ts = fst $ foldl foldFunc ([], 0) ts
    where foldFunc (pairs, i) t = 
              case t of
                Elm _     -> ((i, t): pairs, i + 1)                
                otherwise -> (pairs, i + 1)  -- otherwise, ignore

{- Constructs a set of pairs to filter the records of a set based on the
   positions of an atomic formula's terms that share the same variables.
-}
varPreds :: [Term] -> [(Int, Int)]
varPreds ts = 
    let varMap = createVarMap ts
    in  Map.fold foldPred [] varMap
    where foldPred ps pairs = 
              if length ps > 1
              then (pairsOf ps) ++  pairs
              else pairs
          pairsOf pos        = (\t -> (head pos, t)) <$> (tail pos)

{-| Evaluates a relational expression in a set of tables as the database. -}
evaluateRelExp :: Tables -> RelExp -> DB.Set [Term]
evaluateRelExp _ TblEmpty = DB.Set []
evaluateRelExp _ TblFull  = DB.Set [[Elm "True"]]
evaluateRelExp tbls (Tbl t) = 
    case Map.lookup t tbls of      
      Just t' -> t'
      Nothing -> DB.Set []
evaluateRelExp tbls (Proj exp inds) = 
    if    null (DB.toList projected)
    then  if   (not.null) (DB.toList set)
          then evaluateRelExp tbls TblFull
          else evaluateRelExp tbls TblEmpty
    else  projected
    where set       = evaluateRelExp tbls exp
          projected = DB.project (buildProjPred inds) set
evaluateRelExp tbls (Slct exp vPairs ePairs) =
    DB.select pred set
    where set   = evaluateRelExp tbls exp
          pred  = DB.Pred $ \x -> vPred x && ePred x
          vPred = \x -> and $ (\(p1, p2) -> x !! p1 == x !! p2) <$> vPairs
          ePred = \x -> and $ (\(p,  t ) -> x !! p  == t      ) <$> ePairs
evaluateRelExp tbls (Join exp1 exp2 pairs) = 
    mergeSetPairs $ DB.join pred set1 set2
    where set1  = evaluateRelExp tbls exp1
          set2  = evaluateRelExp tbls exp2
          pred  = DB.Pred $ \(x,y) -> 
                  and $ (\(p1, p2) -> x !! p1 == y !! p2) <$> pairs

-- Helper
{- Returns a list of labels (as needed by _RelExp functions) for a given set
 of terms. -}
termLabels :: [Term] -> Labels
termLabels ts = labelingFunc <$> ts
    where labelingFunc t = case t of 
                             Var v     -> Just v
                             otherwise -> Nothing

{- Creates a list of pairs, representing a join predicate for two set of input
 labels. -}
varJoinPairs :: Labels -> Labels -> [(Int, Int)]
varJoinPairs lLbls rLbls = 
    foldr foldPred [] $ Map.toList lVarMap
    where lVarMap               = createLabelMap lLbls
          rVarMap               = createLabelMap rLbls
          -- Construct two maps for the variables of the two sets of labels and
          -- only choose one of the positions for each variable (head).
          foldPred (v, lp) pairs = 
              case Map.lookup v rVarMap of
                Nothing -> pairs
                Just rp -> (lp, rp): pairs
              -- If a variable from the first map appears in a the second map,
              -- construct a pair to force equality for the two corresponding 
              -- positions.

{- Creates a map from a variable name to its positions in a list of variable
   labels.-}
createLabelMap :: Labels -> Map.Map Var Int
createLabelMap ts = fst $ foldl foldFunc (Map.empty, 0) ts                    
    where foldFunc (m, i) t = 
              case t of
                Just t'    -> (Map.insertWith const t' i m, i + 1)
                Nothing    -> (m, i + 1)

{- Just like createLabelMap, creates a map from a variable name to its 
   positions in a list of variable labels.-}
createVarMap :: [Term] -> Map.Map Var [Int]
createVarMap ts = fst $ foldl foldFunc (Map.empty, 0) ts
    where foldFunc (m, i) t = 
              case t of
                Var t'    -> (Map.insertWith (++) t' [i] m, i + 1)
                othewrise -> (m, i + 1)

{- Merges a pair of tables (wrapped inside one DB.Set) into one table. This 
kind of pairs are usually constructed by the join operation in Henglein's 
library. -}
mergeSetPairs :: DB.Set ([a], [a]) -> DB.Set [a]
mergeSetPairs tbl = mergeFunc <$> tbl
    where mergeFunc = \(a, b) -> (a ++ b)

{- Construct a projection predicate for a given set of indices. -}
buildProjPred :: [Int] -> DB.Proj [Term] [Term]
buildProjPred inds = DB.Proj (indProjList inds)

indProjList :: [Int] -> [a] -> [a]
indProjList ps xs = [xs !! p | p <- ps]

{-| Given a table and its corresponding labels, which represents the left of a 
  sequent, and a set of tables and their labels corresponding to the right of a
  sequent, computes a table consisting of a set of tuples in the first set that 
  are not in the second set. This makes it possible for the chase to compute a 
  set of substitutions in a chase step. -}
diff :: (Table, Labels) -> [(Table, Labels)] -> Table
diff bdy@(DB.Set set, _) hds = -- if hds is empty
    let temp = zip set conds
    in  DB.Set $ fst.unzip $ filter (\t -> snd t == True) temp
    where diffs = (diffHelper bdy) <$> hds
          conds = foldr1 (zipWith (&&)) diffs

{- A helper for diff that evaluates the left against only one of the tables on
 right. -}
diffHelper :: (Table, Labels) -> (Table, Labels) -> [Bool]
diffHelper (bdySet, bdyLbls) (hdSet, hdLbls) = 
    (\t -> not (t `elem` hdSet')) <$> bdySet'
    where (bdyPrj, hdPrj) = sequentProjections bdyLbls hdLbls
          DB.Set bdySet'  = DB.project bdyPrj bdySet
          DB.Set hdSet'   = DB.project hdPrj hdSet

{- A helper for diffHelper that constructs two projection instances, which align
 the columns of the two tables (on left and right of a sequent) in order to make
 tuple comparison possible. It (1) projects the table on right over those 
 attributes that occur in the table on left, and (2) sorts the columns of both 
 tables.-}
sequentProjections :: Labels -> Labels -> 
                    (DB.Proj [Term] [Term], DB.Proj [Term] [Term])
sequentProjections bdyLbls hdLbls = 
    (arrangeLabels bdyLbls' bdyRef, arrangeLabels hdLbls' hdRef)
    where bdyRef   = sortedReference bdyLbls
          hdRef    = sortedReference hdLbls
          bdyLbls' = filterLabels bdyLbls hdLbls
          hdLbls'  = filterLabels hdLbls bdyLbls'
--------------------------------------------------------------------------------
{- Filters a list of labels according to a refrence set of labels. It removes
 the labels that do not show up in the reference set. -}
filterLabels :: Labels -> Labels -> Labels
filterLabels lbls refLbls = 
    filter ((flip elem) refLbls) varLbls
    where varLbls = filter (/= Nothing) lbls

{- Maps every variable in a list of tables to a position corresponding to the 
   variable in a sorted list of variables. -}
sortedReference :: Labels -> Map.Map Var Int
sortedReference lbls =     
    fst $ foldl foldFunc (Map.empty, 0) lbls
    where foldFunc (m, i) (Just v)
                     = (Map.insert v i m, i + 1)
          foldFunc (m, i) Nothing
                     = (m, i + 1)


{- Creates an DB.Proj instance that arranges a set of labels based on a set of
   reference positions maps. It simply ignores the labels that do not show up
   in the reference. -}
arrangeLabels :: Labels -> Map.Map Var Int -> DB.Proj [Term] [Term]
arrangeLabels lbls refMap = DB.Proj $ arrangeLabelsHelper lbls refMap

arrangeLabelsHelper :: Labels -> Map.Map Var Int -> [Term] -> [Term]
arrangeLabelsHelper lbls refMap ts =
    foldr arrangeFunc [] $ filter (/=Nothing) $ sort lbls
    where arrangeFunc (Just l) res = case Map.lookup l refMap of
                                       Nothing -> res
                                       Just p  -> ts !! p: res

{- Reorders the data in a table according to a given pattern. -}
reordList :: [Int] -> [a] -> [a]
reordList _  []     = []
reordList [] _      = []
reordList (p:ps) xs = xs !! p: reordList ps xs

{- Removes duplicate variable names of a set of labels. -}
remDupVars :: Labels -> Labels
remDupVars vs = foldr foldFunc [] vs
    where foldFunc v vs' = if   v `elem` vs'
                           then Nothing:vs'
                           else v:vs'
{- Unions two sets (tables for us). It is implied that the two sets contain 
   unionable columns. -}
unionSets :: Eq a => DB.Set a -> DB.Set a -> DB.Set a
unionSets (DB.Set s1) (DB.Set s2) = DB.Set $ union s1 s2

{- Merges two sets of tables. The tables with the same key will be unioned. -}
mergeSets :: Tables -> Tables -> Tables
mergeSets t1 t2 =  Map.unionWith unionSets t1 t2

{- Merges a list of tables. The tables with the same key will be unioned. -}
mergeAllSets :: [Tables] -> Tables
mergeAllSets ts = Map.unionsWith unionSets ts

--------------------------------------------------------------------------------