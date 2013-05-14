{- Time-stamp: <Mon 11/12/12 11:35 Dan Dougherty CC.hs>
   Implements an algorithm for computing congruence closure based on
   Bachmair, Tiwari and Vigneron's (BTV) rewriting transformations.
-}

module CC.CC' where

import Control.Applicative

import Text.ParserCombinators.Parsec hiding ( (<|>) )
import Text.Parsec.Token ( TokenParser )
import qualified Text.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language ( haskellStyle )
import qualified Text.ParserCombinators.Parsec.Expr as Expr

import Data.List
import qualified  Data.Map
import Data.Maybe
import Debug.Trace (trace)
import Control.Exception -- for assert


-- Logic modules:
import Formula.SyntaxFol
import Utils.FolUtilities
import Tools.Unification
import Tools.Equality


-- A rewrite rule in the form of a D-rule or a C-rule
data RWRule = RW Term Term
              deriving (Show, Eq)

-- An equation on two terms
data Equation = Eql Term Term 
                deriving (Show, Eq)


type TRS = [RWRule]

-- A rewrite system representing the state of the computation.
data RWState = RWSt {
    stConstants :: [Sym], 
    stEquations :: [Equation], 
    stRules :: TRS
    } deriving Show

-- Updates a rewrite system with a list a new equations. It applies
-- cc on the rewrite system and retrurns the new rewrite system.
-- This method returns an initial rewrite system for a given set of
-- equations if the second arguemtn is a pair of empty lists.
-- Inputs: new equations :: [Equation]
--         old rewrite system and its constants :: (TRS, [Sym])
-- Output: new rewrite system and its constants :: (TRS, [Sym])
buildTRS :: [Equation] -> (TRS, [Sym]) -> (TRS, [Sym])
buildTRS eqs (sys, cs)  = 
    (stRules new, stConstants new)
    where new = cc (RWSt cs eqs sys)


-- Computes a congruence closure for a set of equations according to
-- Shotak's algorithm: ((sim*.ext?)*.(del + ori).(col.ded*)*)*
cc :: RWState -> RWState
cc sys@(RWSt _ [] _) = sys
cc sys@(RWSt consts (eq:eqs) rs) =
    -- (trace ("--" ++ (show (eq:eqs))))
    -- (trace ("++" ++ (show rs)))
    (if t1' == t2'
    then cc (RWSt consts' eqs rs')
    else cc (RWSt consts' eqs'' (rule:rs'')))
    where (consts', eq'@(Eql t1' t2'), rs') = simsexts consts rs eq
          (eqs'', rs'') = coldedss consts rule (eqs) rs'
          rule = (ori consts' eq')



--------------------------------
-- Steps
--------------------------------
simsexts :: [Sym] -> TRS -> Equation -> ([Sym], Equation, TRS)
simsexts consts rs eq =
    case extResult of
      Nothing -> (consts, eq', rs)
      Just (consts'', eq'', rs'') -> simsexts consts'' (union rs rs'') eq''
    where eq' = sims rs eq
          extResult = ext consts eq' rs


-- TODO: the performance is can be improved
coldedss :: [Sym] -> RWRule -> [Equation] -> TRS -> ([Equation], TRS)
coldedss _ _ eqs [] = (eqs, [])
coldedss consts rule eqs (r:rs) = 
    -- if r == r' -- no collapse for this rule
    -- then (union dedEqs restEqs, r':(union dedRules restRules))
    -- else (union dedEqs restEqs, r':(union dedRules restRules))
    (union dedEqs restEqs, r':(union dedRules restRules))
    where r' = col consts rule r
          (dedEqs, dedRules) = deds (r':rs)
          (restEqs, restRules) = coldedss consts rule eqs rs

--------------------------------
-- Simplification
--------------------------------
-- Applies sim* on an equation until the equation doesn't change.
-- (Question: do we ever need to apply a rule twice?)
sims :: TRS -> Equation -> Equation
sims rs eq = if eq == eq' 
             then eq 
             else sims rs eq'
    where eq' = sim rs eq
-- Applies a simplification step on an equation for a given set of rewrite rules.
sim :: TRS -> Equation -> Equation
sim rs eq = foldr (\r e -> (onEquation (simTerm r)) e) eq rs

-- Simplieifes a term w.r.t a rewrite rule
simTerm :: RWRule -> Term -> Term
simTerm _ (Var _) = error "simTerm: the input term must be ground!"
simTerm r@(RW t1 t2) t@(Fn f ts)
        | t == t1 = t2 -- Question: what if r applies on t2?
        | otherwise = Fn f (map (simTerm r) ts)

--------------------------------
-- Extension
--------------------------------
-- Applies one extension step on an euqation. It tries to extend the term on
-- left first; if it cannot find any extension for the term on left, extends
-- the term on right. Also, within each term, it extends its subterms from
-- left to right and returns if an extension is applicable; if none of the
-- subterms can be extended, it tries to extend the term itself.
ext :: [Sym] -> Equation -> TRS -> Maybe ([Sym],Equation, TRS)
ext consts eq@(Eql t1 t2) rules =
    case (ext_t1, ext_t2) of
      (Nothing, Nothing) -> Nothing 
      (Just (t1', c', rules'), _) -> Just (c':consts, 
                                             Eql t1' t2, rules')
      (Nothing, Just (t2', c', rules')) -> Just (c':consts, 
                                                   Eql t1 t2', rules')
    where ext_t1 = extTerm consts t1 rules
          ext_t2 = extTerm consts t2 rules

extTerm :: [Sym] -> Term -> TRS -> Maybe (Term, Sym, TRS)
extTerm _ (Var _) _ = error "extTerm: the input term must be ground!"
extTerm consts term@(Fn f []) rules = 
    if f `elem` consts
    then Nothing
    else Just (new, sym, [RW term new])
    where new = Fn sym []
          sym = freshConstant (length consts)
extTerm consts term@(Fn f terms) rules = 
    case extSubterms rules consts terms of
      Nothing -> Just (new, sym, [RW term new])
      Just (terms', sym', rules') -> Just (Fn f terms', sym', rules')
    where new = Fn sym []
          sym = freshConstant (length consts)


-- This function extends the firs term that can be extended and 
-- keeps the rest unchanged. It returns the new list of terms,
-- the new symbol replacing the extended term, and the new TRS
-- if such extension exists. Otherwise, returns Nothing.
extSubterms :: TRS -> [Sym] -> [Term] -> Maybe ([Term], Sym, TRS)
extSubterms rules consts terms =
    if flg' == True 
    then Just (ts', sym', rs')
    else Nothing
    where (flg', ts', sym', rs') = 
              -- flg: determines whether a term has already been extended or not.
              -- ts: will contain the new terms (where only the first term that
              -- can be extended is extended) when the folding is over.
              -- s: will hold the string representation of the newly extended 
              -- constant.
              -- rs: contains the rewrite rules and will be updated according to
              -- the new extension.
              foldr (\t (flg, ts, s, rs) -> 
                  (case flg of
                     False -> (case extTerm consts t rules of
                                 Nothing -> (False, t:ts, s, rs)
                                 Just (t', s', rs') -> (True, t':ts, s', rs'))
                     True -> (True, t:ts, s, rs))) (False,[],"c",rules) terms

--------------------------------
-- Deletion
--------------------------------

--------------------------------
-- Orientation
--------------------------------
-- Creates a rewriting rule corresponding to an equation based on an 
-- orientation transformation.
ori :: [Sym] -> Equation -> RWRule
ori consts (Eql t1 t2)
    | sym1 `elem` consts && sym2 `elem` consts =
      if t1 > t2
      then RW t1 t2
      else RW t2 t1
    | sym1 `elem` consts = RW t2 t1
    | sym2 `elem` consts = RW t1 t2
    | otherwise = error "ori: this wasn't expected!"
    where Fn sym1 _ = t1
          Fn sym2 _ = t2

--------------------------------
-- Collapse
--------------------------------
-- Collapses a rewrite rule according to another rule. Note that it
-- only collapses the left of the rule.
col :: [Sym] -> RWRule -> RWRule -> RWRule
col consts rule@(RW rt1 rt2) r@(RW t1 t2) = --RW (colTerm rule t1) t2
    if (greater t2 t1') || (t1 == t1') -- follow term ordering
    then r -- do not collapse this rule
    else r'
    where r'@(RW t1' _) = RW (colTerm rule t1) t2
          greater (Fn x []) (Fn y []) = 
              case (x `elem` consts, y `elem` consts) of
                (True, True) -> x > y
                (False, False) -> x > y
                (False, True) -> True
                (True, False) -> False
          greater x@(Fn x' _) y@(Fn y' []) =
              case y' `elem` consts of
                True -> True
                False -> x > y
          greater x@(Fn x' []) y@(Fn y' _) =
              case x' `elem` consts of
                True -> False
                False -> x > y
          greater x y = x > y


-- Collapses a term w.r.t a rewrite rule (happens to be similar to simTerm)
-- TODO: unify this function with simTerm
colTerm :: RWRule -> Term -> Term
colTerm _ (Var _) = error "simTerm: the input term must be ground!"
colTerm r@(RW t1 t2) t@(Fn f ts)
        | t == t1 = t2 -- Question: what if r applies on t2?
        | otherwise = Fn f (map (colTerm r) ts)

--------------------------------
-- Deduction
--------------------------------
deds :: TRS -> ([Equation], TRS)
deds [] = ([], [])
deds (r@(RW t1 t2):rs) =
    -- (trace ("++" ++ (show (r:rs))))
    (case r' of
      Nothing -> ([], r:restRules)
      Just (RW t1' t2') -> ((Eql t2 t2'):restEqs, restRules))
    where r' = find (\(RW x y) -> x == t1) rs 
          (restEqs, restRules) = deds rs

--------------------------------
-- Test Equality
--------------------------------
-- Returns the normal form of a term according to a given rewrite system.
normalForm :: TRS -> Term -> Term
normalForm _ v@(Var x) = v
normalForm rs t@(Fn f ts) =
    (let rule = find (\(RW t1@(Fn rf rts) t2) -> 
                         (Fn rf (nfs rts)) == (Fn f (nfs ts))) rs in
    case rule of
      Nothing -> t
      Just (RW _ t') -> normalForm rs t')
    where nfs x = map (normalForm rs) x

-- Returns true if the two terms have the same normal form in the given
-- rewrite system; otherwise, returns false.
equalTerms :: TRS -> Term -> Term -> Bool
equalTerms rs t1 t2 = (normalForm rs t1) == (normalForm rs t2)
--------------------------------
-- Helpers
--------------------------------
-- Applies a function on an equation
onEquation :: (Term -> Term) -> Equation -> Equation
onEquation f (Eql t1 t2) = Eql (f t1) (f t2)

-- Applies a function on a rewrite rule
onRWRule :: (Term -> Term) -> RWRule -> RWRule
onRWRule f (RW t1 t2) = RW (f t1) (f t2)

-- Creates a fresh constant
freshConstant :: Int -> Sym
freshConstant counter = "const" ++ (show counter)
--------------------------------
-- Tests
--------------------------------
testEqual :: TRS -> String -> String -> Bool
testEqual rs str1 str2 = equalTerms rs (parseTerm str1) (parseTerm str2) 

makeEql :: String -> String -> Equation
makeEql str1 str2 = Eql (parseTerm str1) (parseTerm str2)

makeRW :: String -> String -> RWRule
makeRW str1 str2 = RW (parseTerm str1) (parseTerm str2)


test_eq1 = makeEql "a()" "b()"
test_eq2 = makeEql "c()" "b()"
test_eq3 = makeEql "f(a())" "b()"
test_eq4 = makeEql "g(f(a()))" "h(a(), c())"
test_eq5 = makeEql "g(b())" "d()"


test_eqs1 = [test_eq1, test_eq2]
test_eqs2 = [test_eq2, test_eq3]
test_eqs3 = [test_eq2, test_eq3, test_eq4, test_eq5]

test_dd = [makeEql "f(f(f(a())))"  "a()" ,makeEql "f(f(f(f(f(a())))))"  "a()",makeEql "a()"  "d()" , makeEql "g(h(a()))"  "a()" ,makeEql "g(m(a()))"  "a()" ,makeEql "h(a())"  "c()" , makeEql "m(g(c()))"  "b()"]

test_tst = [(makeEql "f(f(f(a())))" "a()" )]

go eqs = cc (RWSt [] eqs [])