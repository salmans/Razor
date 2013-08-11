{- Time-stamp: <Mon 11/12/12 11:35 Dan Dougherty CC.hs>
   Implements an algorithm for computing congruence closure based on
   Bachmair, Tiwari and Vigneron's (BTV) rewriting transformations.
-}

module CC.Shostak where

import Control.Applicative

import Data.List
import qualified  Data.Map
import Data.Maybe
import Control.Exception -- for assert
import Debug.Trace

import qualified Control.Monad.State as State

-- Logic modules:
import Formula.SyntaxGeo
import Utils.GeoUtilities
import Tools.GeoUnification


-- A rewrite rule in the form of a D-rule or a C-rule
data RWRule = RW Term Term
              deriving (Show, Eq)

instance TermBased RWRule where
    liftTerm f (RW t1 t2) = RW (f t1) (f t2)
    freeVars (RW t1 t2) = union (freeVars t1) (freeVars t2)

-- An equation on two terms
data Equation = Eql Term Term 
                deriving (Show, Eq)


instance TermBased Equation where
    liftTerm f (Eql t1 t2) = Eql (f t1) (f t2)
    freeVars (Eql t1 t2) = union (freeVars t1) (freeVars t2)



type TRS = [RWRule] -- A Term Rewrite System

type CCModel = (TRS, [Term])

type CountState = State.State Int
increment :: CountState Term
increment =
    do
      counter <- State.get
      State.put (counter + 1)
      return $ freshConstant counter
      

-- A rewrite system representing the state of the computation.
data RWState = RWSt {
    stEquations :: [Equation] 
    , stModel   :: CCModel
    , stRules   :: [RWRule]
    } deriving Show

-- Updates a rewrite system with a list a new equations. It applies
-- cc on the rewrite system and retrurns the new rewrite system.
-- This method returns an initial rewrite system for a given set of
-- equations if the second arguemtn is a pair of empty lists.
-- Inputs: new equations :: [Equation]
--         old rewrite system and its constants :: (TRS, [Term])
-- Output: new rewrite system and its constants :: (TRS, [Term])
buildTRS :: [Equation] -> CCModel -> (CCModel, [RWRule])
buildTRS eqs (trs, cs)  = 
    let (rwSt, counter) = State.runState (cc $ RWSt eqs (trs,cs) []) (length cs)
        (RWSt _ (newTRS, newCs) newRules) = rwSt
    in  ((newTRS, newCs) , newRules)


-- Computes a congruence closure for a set of equations according to
-- Shostak's algorithm: ((sim*.ext?)*.(del + ori).(col.ded*)*)*
cc :: RWState -> CountState RWState
-- cc = garbageCollect.com.ccHelper
cc = \x -> ccHelper x >>= com >>= garbageCollect

ccHelper :: RWState -> CountState RWState
ccHelper st@(RWSt [] _ newRules) = return st
ccHelper st@(RWSt (eq:eqs) (trs,cs) rs) =
    do
      (cs', eq'@(Eql t1' t2'), trs', rs') <- 
            simsexts cs trs eq rs  -- apply (sim*.ext?)*
      (rule, rs'') <- ori cs' eq' rs'
      (eqs''', trs''', rs''') <- 
            coldedss cs rule (eqs) trs' rs'' -- apply (col.ded*)*
      if t1' == t2' 
        then ccHelper (RWSt eqs (trs', cs') rs') -- recurse
           -- ignore the result of ext (del) if eq' is trivial, then recurse
        else ccHelper (RWSt eqs''' (rule:trs''', cs') rs''')
           -- recurse
          
--------------------------------
-- Steps
--------------------------------
simsexts :: [Term] -> TRS -> Equation -> [RWRule] -> 
            CountState ([Term], Equation, TRS, [RWRule])
simsexts cs trs eq rs =
    do
      eq' <- sims trs eq -- apply sim*
      ex  <- ext cs eq' trs rs -- apply (ext?)
      case ex of
        Nothing -> return (cs, eq', trs, rs)
        Just (cs'', eq'', trs'', rs'') -> 
            simsexts cs'' (union trs trs'') eq'' rs'' -- recurse

-- TODO: the performance can be improved
coldedss :: [Term] -> RWRule -> [Equation] -> TRS -> [RWRule] -> 
            CountState ([Equation], TRS, [RWRule])
coldedss _ _ eqs [] rs = return (eqs, [], rs)
coldedss cs rule eqs (r:trs) rs = 
    do
      (r', rs') <- col cs rule r rs -- apply col
      (eqs'', trs'', rs'') <- deds (r':trs) rs' -- apply ded*
      (eqs''', trs''', rs''') <- coldedss cs rule eqs trs rs'' -- recurse
      return (eqs''', r':trs''', rs''') -- collect results
        -- EXPERIMENT
        -- (union eqs'' eqs''', r':(union trs'' trs'''), rs''')

--------------------------------
-- Simplification
--------------------------------
{- Applies sim* on an equation until the equation doesn't change. -}
sims :: TRS -> Equation -> CountState Equation
sims rs eq | eq == eq' = return eq
           | otherwise = sims rs eq'
    where eq' = sim rs eq

{- Applies a simplification step on an equation for a given set of rules. -}
sim :: TRS -> Equation -> Equation
sim = flip $ foldr (liftTerm.simTerm)

-- Simplieifes a term w.r.t a rewrite rule
simTerm :: RWRule -> Term -> Term
simTerm _ (Var _) = error "simTerm: the input term must be ground!"
simTerm r@(RW t1 t2) t@(Fn f ts)
        | t == t1 = t2
        | otherwise = Fn f $ map (simTerm r) ts
simTerm r@(RW t1 t2) t@(Elm _)
        | t == t1 = t2
        | otherwise = t

--------------------------------
-- Extension
--------------------------------
-- Applies one extension step on an euqation. It tries to extend the term on
-- left first; if it cannot find any extension for the term on left, extends
-- the term on right. Also, within each term, it extends its subterms from
-- left to right and returns if an extension is applicable; if none of the
-- subterms can be extended, it tries to extend the term itself.
ext :: [Term] -> Equation -> TRS -> [RWRule] -> 
       CountState (Maybe ([Term],Equation, TRS, [RWRule]))
ext consts eq@(Eql t1 t2) rules newRules =
    do
    ex1 <- extTerm consts t1 rules
    ex2 <- extTerm consts t2 rules
    return $ case (ex1, ex2) of
               (Just (t1', c', rules'), _                     ) ->
                   -- the term on left can be extended
                   Just (c':consts, Eql t1' t2, rules', newRules ++ rules')
               (Nothing               , Just (t2', c', rules')) ->
                   -- only the term on right can be extended
                   Just (c':consts, Eql t1 t2', rules', newRules ++ rules')
               otherwise                                        -> 
                   -- neither of the terms can be extended
                   Nothing

extTerm :: [Term] -> Term -> TRS -> CountState (Maybe (Term, Term, TRS))
extTerm _ (Var _) _ = error "extTerm: the input term must be ground!"
extTerm consts elm@(Elm _) rules = 
    if elm `elem` consts
    then return Nothing
    else if elm == Elm "True"
         then return $ Just (elm, elm, []) -- Elm "True" is a special case
         else (do
                --counter <- increment
                --let freshC = freshConstant (length consts)
                freshC <- increment
                return $ Just (freshC, freshC, [RW elm freshC]))
extTerm consts term@(Fn f terms) rules = 
    do
      ex <- extSubterms rules consts terms
      case ex of
        Nothing                     -> 
            (do
              --counter <- increment
              --let freshC = freshConstant (length consts)
              freshC <- increment
              return $ Just (freshC, freshC, [RW term freshC]))
        Just (terms', sym', rules') -> 
            return $ Just (Fn f terms', sym', rules')
    where freshC = freshConstant (length consts)


-- <<ALERT>>
-- The following code is extremely inefficient and unreadable.
-- Since the code is completely thrown away in the new incremental
-- implementation of the chase, don't bother changing it!

-- This function extends the firs term that can be extended and 
-- keeps the rest unchanged. It returns the new list of terms,
-- the new symbol replacing the extended term, and the new TRS
-- if such extension exists. Otherwise, returns Nothing.
extSubterms :: TRS -> [Term] -> [Term] -> 
               CountState (Maybe ([Term], Term, TRS))
extSubterms rules consts terms =
    do
      (flg', ts', sym', rs') <- folded
      if flg' == True 
        then return $ Just (ts', sym', rs')
        else return Nothing
    where folded= 
              -- flg: determines whether a term has already been extended or not.
              -- ts: will contain the new terms (where only the first term that
              -- can be extended is extended) when the folding is over.
              -- s: will hold the string representation of the newly extended 
              -- constant.
              -- rs: contains the rewrite rules and will be updated according to
              -- the new extension.
              foldr (\t soFar -> 
                  (do
                    (flg, ts, s, rs) <- soFar
                    ex <- extTerm consts t rules
                    case flg of
                      False -> (case ex of
                                  Nothing -> return (False, t:ts, s, rs)
                                  Just (t', s', rs') -> return (True, t':ts, s', rs'))
                      True -> return (True, t:ts, s, rs))) 
              (return (False,[],Elm "c",rules))
              terms

--------------------------------
-- Deletion
--------------------------------

--------------------------------
-- Orientation
--------------------------------
-- Creates a rewriting rule corresponding to an equation based on an 
-- orientation transformation.
ori :: [Term] -> Equation -> [RWRule] -> CountState (RWRule, [RWRule])
ori consts (Eql t1 t2) newRules
    | t1 `elem` consts && t2 `elem` consts =
      if t1 > t2
      then return (RW t1 t2, RW t1 t2: newRules)
      else return (RW t2 t1, RW t2 t1: newRules)
    | t1 `elem` consts = return (RW t2 t1, RW t2 t1: newRules)
    | t2 `elem` consts = return (RW t1 t2, RW t1 t2: newRules)
    | otherwise = error "ori: this wasn't expected!"

--------------------------------
-- Collapse
--------------------------------
-- Collapses a rewrite rule according to another rule. Note that it
-- only collapses the left of the rule.
col :: [Term] -> RWRule -> RWRule -> [RWRule] -> CountState (RWRule, [RWRule])
col consts rule@(RW rt1 rt2) r@(RW t1 t2) newRules = --RW (colTerm rule t1) t2
    if (greater t2 t1') || (t1 == t1') -- follow term ordering
    then return (r, newRules) -- do not collapse this rule
    else return (r', r':delete r newRules)
    where r'@(RW t1' _) = RW (colTerm rule t1) t2
          greater x y = 
              case (x `elem` consts, y `elem` consts) of
                (True, True) -> x >= y
                (False, False) -> x >= y
                (False, True) -> True
                (True, False) -> False


-- Collapses a term w.r.t a rewrite rule (happens to be similar to simTerm)
colTerm :: RWRule -> Term -> Term
colTerm _ (Var _) = error "simTerm: the input term must be ground!"
colTerm r@(RW t1 t2) t@(Fn f ts)
        | t == t1 = t2 -- Question: what if r applies on t2?
        | otherwise = Fn f (map (colTerm r) ts)
colTerm r@(RW t1 t2) t@(Elm _)
        | t == t1 = t2
        | otherwise = t

--------------------------------
-- Deduction
--------------------------------
deds :: TRS -> [RWRule] -> CountState ([Equation], TRS, [RWRule])
deds [] newRules = return ([], [], newRules)
deds (r@(RW t1 t2):rs) newRules =
    do 
      (restEqs, restRules, newRules') <- deds rs newRules
      case r' of
        Nothing -> return ([], r:restRules, newRules')
        Just (RW t1' t2') -> return ((Eql t2 t2'):restEqs, restRules, delete r newRules')
    where r' = find (\(RW x y) -> x == t1) rs 
          

--------------------------------
-- Composition
--------------------------------
-- Composes two rules in the TRS
-- The way that composition works is not ideal but  since we want to implement a
-- new congruence closure algorithm, this approach is fine for now.
com :: RWState -> CountState RWState
com state@(RWSt _ (rules, _) _) = 
    comHelper rules state

comHelper :: TRS -> RWState -> CountState RWState
comHelper trs (RWSt eqs (rs, cs) newRules) = 
    return $ RWSt eqs ((nub.normalize) rs,cs) ((nub.normalize) newRules)
    where normalize = foldr (\(RW t1 t2) res -> 
                                 (RW t1 (normalForm trs t2)):res) []

-- comHelper _ st@(RWSt eqs ([], cs) newRules) = st
-- comHelper allRules (RWSt eqs (r@(RW t1 t2):rs, cs) newRules) =
--     (case r' of
--       Nothing -> RWSt restEqs (r:restRules,cs) newRules'
--       Just (RW t1' t2') -> RWSt eqs ((RW t1 t2'):(delList rs), cs)
--                             $ union [RW t1 t2'] (delList newRules'))
--     where r' = find (\(RW x y) -> x == t2) allRules
--           RWSt restEqs (restRules,_) newRules' = 
--               comHelper allRules (RWSt eqs (rs, cs) newRules)
--           delList l = deleteBy (\(RW x _) (RW y _) -> x == y) (RW t1 t2) l
--------------------------------
-- Garbage Collection
--------------------------------
-- In the next congruence closure implementation, we should be handle garbage
-- collection within the algorithm as a part of transformations.
garbageCollect :: RWState -> CountState RWState
garbageCollect state@(RWSt eqs (rs, cs) nrs) =
    return $ RWSt eqs (rs', cs) nrs
    where rs' = foldr (\r@(RW x y) rules ->
                           if (x `elem` cs) && 
                              (y `elem` cs) && 
                              (y /= Elm "True") -- not great!
                           then rules
                           else r:rules) [] rs


--------------------------------
-- Test Equality
--------------------------------
{- Tries to perform a single rewrite step at the root of t by examining the 
rules (l,r) in order -}
rewrite :: TRS -> Term -> Maybe Term
rewrite [] t = Nothing
rewrite ((RW l r):rest) t = 
    case match [(l, t)] of 
      -- We don't need unification because we have a ground TRS.
      Nothing -> rewrite rest t
      Just s  -> Just $ liftTerm (lift s) r

{-| Returns the normal form of a term according to a given rewrite system. -}
normalForm :: TRS -> Term -> Term
normalForm _ (Var x)     = Var x
normalForm trs (Fn f ts) =
    let u = Fn f (map (normalForm trs) ts)
    in  case rewrite trs u of
          Nothing -> u
          Just t  -> normalForm trs t
normalForm trs (Elm e)   = 
    let u = Elm e
    in  case rewrite trs u of
          Nothing -> u
          Just t  -> normalForm trs t

-- Returns true if the two terms have the same normal form in the given
-- rewrite system; otherwise, returns false.
equalTerms :: TRS -> Term -> Term -> Bool
equalTerms rs t1 t2 = (normalForm rs t1) == (normalForm rs t2)
--------------------------------
-- Helpers
--------------------------------

-- Creates a fresh constant
freshConstant :: Int -> Term
freshConstant counter = Elm $ "const" ++ (show counter)