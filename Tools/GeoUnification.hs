{- Time-stamp: <Wed 11/14/12 11:42 Dan Dougherty Unification.hs>
   Unification, following a Martelli-Montanari-style transformations approach.
-}

module Tools.GeoUnification where
-- 
import qualified Data.Map as Map
import Data.Maybe
import Control.Exception -- for assert
-- import Debug.Trace
import Data.List
--
import Formula.SyntaxGeo
import Utils.GeoUtilities(Sub)
import qualified Utils.GeoUtilities 
type PairList = [(Term, Term)]
type State    = (PairList, Sub)



-- the result of applying a substitution to a term.  Note that partial
-- application of this function turns a substitution as a Data.Map into
-- an ordinary Haskell function on terms; this is sometimes useful.
liftSub :: Sub -> Term -> Term
liftSub sub trm =
    case trm of
      Var x -> case (Map.lookup x sub) of
            Just t' -> t'
            Nothing -> trm
      Fn f args  -> Fn f (map (liftSub sub) args)
      Elm _ -> trm
 

-- display a substitution is a readable way   
prettySub :: Sub -> IO ()
prettySub sub =
 let sublist = Map.toList sub
     newlist = map (\(v,t) -> v++" |-> "++ (show t)) sublist
 in case newlist of
      [] -> putStrLn "the empty substitution" 
      _ -> putStr (unlines (map show newlist))

-- =====================================

-- matching function
-- Returns a sub (if exists) that matches firsts of the input pairs of terms
-- to the seconds of the pairs.
match :: PairList -> Maybe Sub
match pairs = matchLoop (pairs, Map.empty)

matchLoop :: State -> Maybe Sub
matchLoop ([], sub) = Just sub
matchLoop ((Fn f fargs, Fn g gargs):rest, sub)
          | (f == g) && ((length fargs) == (length gargs)) =
              matchLoop ((zip fargs gargs) ++ rest, sub)
          | otherwise = Nothing
matchLoop ((Elm x, Elm y):rest, sub)
          | (x == y) = matchLoop (rest, sub)
          | otherwise = Nothing
matchLoop ((Var x, t):rest, sub) = 
    case Map.lookup x sub of
      Nothing -> matchLoop (rest, (Map.insert x t sub))
      Just t' | t == t' -> matchLoop (rest, sub)
              | otherwise -> Nothing
matchLoop _ = Nothing

-- =====================================

-- Recursively matches the subterms of the second term with the first term and
-- returns all the possible substitutions.
matchSubterms :: Term -> Term -> [Sub]
matchSubterms t1 t2@(Var x) = 
    case match[(t2,t1)] of
      Nothing -> []
      Just sub -> [sub]
matchSubterms t1 t2@(Elm x) = 
    case match[(t2, t1)] of
      Nothing -> []
      Just sub -> [sub]
matchSubterms t1 t2@(Fn _ []) =
    case match [(t2,t1)] of
      Nothing -> []
      Just sub -> [sub]
matchSubterms t1 t2@(Fn _ terms)=
    case match [(t2,t1)] of
      Nothing -> concatMap (matchSubterms t1) terms
      Just sub -> [sub]

-- =====================================
-- the main unification function
unify :: PairList -> Maybe Sub
unify pairs = loop (pairs, Map.empty)
 
-- Implements the transformations from class, except that it separates
-- the pairs solved so far into a substitution (ie a Map)
--
-- (loop assumes that a function will never be applied to argument lists
-- of different lengths.)
loop :: State -> Maybe Sub
loop (pairs, sub) = --undefined
    case pairs of 
      [] -> Just sub

      ((Var x), t):rest 
       | t == (Var x)  ->  loop (rest, sub)
       | otherwise     ->  elim x t (rest, sub)

      (t , Var x):rest ->  elim x t (rest, sub)

      (Fn f fargs, Fn g gargs): rest 
          | f==g      -> 
              assert (length fargs == length gargs) $
              loop ( (zip fargs gargs)++rest, sub )
          | otherwise -> Nothing
              
-- A helper for loop: Variable Elimination is the trickiest part
--
-- (elim assumes that var is not in the domain of sub; this will be
-- true whenever loop calls this.)
elim :: Var -> Term -> State -> Maybe Sub
elim var trm (pairs, sub) =
    if elem var (Utils.GeoUtilities.termVars trm) then Nothing
    else 
        let
          -- substituting trm for var
          var_trm_sub = Map.fromList [(var, trm)]   :: Sub
          -- make it a function, to apply it
          var_trm_fn  = liftSub var_trm_sub       ::  Term->Term 
          -- apply it to the other pairs
          newPairs  = map (\(t1,t2) -> (var_trm_fn t1, var_trm_fn t2)) pairs ::[(Term,Term)]
          -- apply it to the sub-so-far
          newSub    = Map.union (Map.map var_trm_fn sub) var_trm_sub    ::Sub
        in loop (newPairs, newSub)

--
-- testing
--
io_unify :: PairList -> IO ()
io_unify pairs =
    case unify pairs of
      Nothing -> putStrLn "no unifiers"
      Just sub -> prettySub sub


-- t1 = parseTerm "f(x0,x0)"
-- t2 = parseTerm "f(x1,x1)"
-- t3 = parseTerm "g(f(g(x), f(y,u)))"
-- t4 = parseTerm "g(f(u, f(x,g(z))))"
-- t5 = parseTerm "g(f(u, f(x,x))))"

-- pairs1 = [(t1,t2)]
-- pairs2 = [(t1,t3)]
-- pairs3 = [(t3,t4)]
-- pairs4 = [(t3,t5)]

-- a1 = io_unify pairs1
-- a2 = io_unify pairs2
-- a3 = io_unify pairs3
-- a4 = io_unify pairs4

{- answers for the above tests 
*Unif> a1
"x0 |-> x1"
*Unif> a2
no unifiers
*Unif> a3
"u |-> g(z)"
"x |-> z"
"y |-> z"
*Unif> a4
no unifiers

-}