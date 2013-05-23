{- Time-stamp: <2013-05-15 12:36:47 Salman Saghafi>
   Unification, following a Martelli-Montanari-style transformations approach.
-}

module Tools.Narrowing where
-- 
import qualified Data.Map as Map
import Data.Maybe
import Control.Exception -- for assert
import Debug.Trace
import Data.List
--
import Formula.SyntaxGeo

import Utils.GeoUtilities
import Tools.GeoUnification
import CC.CC


truth = Elm "True" -- Not ideal!

{-| Uses narrowTerm to create a set of narrowing substitutions for the two sides of an equation.
-}
{- The output set of substitutions are guaranteed to lead to a useful 
   instantiation of the corresponding frame; that is, they are returned only if 
   they reduce reduce the two sides of the equation to the same symbol in the 
   rewrite system.

   REMARK: this function puts slightly increases the overhead of narrowing in order 
   to reduce the number of instantiated frames.
-}
narrowEquation :: Bool -> TRS -> [RWRule] -> Term -> Term -> [Sub]
narrowEquation isFact trs rs t1 t2 = 
    filter (\s -> normalForm trs (lift s t1)  == normalForm trs (lift s t2)) 
           -- Return only those substitutions that make equal temrs 
           -- on both sides.
           $ concatMap subs narrowLeft
    where narrowLeft  = narrow isFact trs rs t1
                        -- Results of narrowing applied on t1
          narrowRight (t, s) = [s' | (t',s') <- narrow isFact trs rs (lift s t2)
                               , t == t']
                        -- Apply narrowing on (lift s t2) where t2 is a 
                        -- substitution resulted from narrowing t1. It returns
                        -- a list of substitutions.
          subs (t,s) = case narrowRight (t,s) of
                         [] -> [s]
                         ts -> map (\s' -> Map.union s s') ts
                         -- If there is any substitutions from narrowing right,
                         -- compose them with the substitutions from narrowing
                         -- left, otherwise, only return left narrowings.

{-| Applies narrowTermForRule for a given *set* of rules:
-- Inputs:
-- Param1: is the term representing a fact in the rewrite system?
-- Param2: the original rewrite system under which being a fact is
-- checked.
-- Param3: the set of rewrite rules
-- Param4: the term to narrow
-}
{- The output set of substitutions are guaranteed to lead to a useful 
   instantiation of the corresponding frame; that is, they are returned only if 
   they reduce the input fact to True.
   (2) reduce the two sides of the equation to the same symbol in the rewrite system.

  REMARK: this function puts slightly increases the overhead of narrowing in order to reduce the number of instantiated frames.
-}

narrowTerm :: Bool -> TRS -> [RWRule] -> Term -> [(Term, Sub)]
narrowTerm isFact trs rs t = 
    filter (\(_, s) -> normalForm trs (lift s t) == truth)
               $ narrow isFact trs rs t


narrow :: Bool -> TRS -> [RWRule] -> Term -> [(Term, Sub)]
narrow isFact trs rs t = 
    concatMap (\r -> narrowTermForRule isFact trs r t) rs

-- Applies a narrowing step for a term for a given rewrite rule. That is, it 
-- **matches** a position in the term with the left of the rule, then replaces
-- the subterm in the given position with the term resulted form application
-- of the substitution on the right of the rule.

-- REMARK: if the first parameter is True, the input term is a fact. So, it can 
-- be matched with terms that rewrite to truth in the rewrite system.

-- REMARK: in our rewrite rules, both left and right are ground. Therefore,
-- (1) we don't need to apply the matching substitution on right, and (2) the 
-- narrowing step does not apply more than once.

-- REMARK: since we compute the normal form of right because we need to tell if
-- the terms in the rewrite rule correspond to a fact or a term denotation, we
-- return the normal form instead of right. Thus, we don't need to compute the
-- normal form in a normalization process in the main algorithm (the chase).
narrowTermForRule :: Bool -> TRS -> RWRule -> Term -> [(Term, Sub)]
narrowTermForRule isFact trs rule@(RW l r) t@(Fn f ts) = 
    case match [(t, l)] of -- Does the current position match to rule's left?
      Nothing -> subts     
          -- It doesn't! Get the results for subpositions.
      Just s | isFact && (nf == truth) || 
               (not isFact) && (nf /= truth) -> (nf, s):subts 
             | otherwise -> subts
          -- It does! construct the replacing term and its corresponding 
          -- substitution.
    where subts = [(Fn f ts', s) | (ts', s) <- narrowSubterms trs [] ts rule]
          nf = normalForm trs r
narrowTermForRule isFact trs rule@(RW l r) t = 
    -- takes care of flat Var and Elm terms
    case match [(t, l)] of
      Nothing -> []
      Just s | isFact && (nf == truth) ||
               (not isFact) && (nf /= truth) -> [(nf, s)]
             | otherwise -> []
    where nf = normalForm trs r

-- This is a helper function for narrowTermForRule. Instead of accepting only 
-- one term, it accepts a list of terms and applies a narrowing step on every
-- term. The result, is a list of list of terms such that in each sublist,
-- only one of the terms are narrowed but the terms on left and right of
-- that term are just the input terms as they were passed to this function.
narrowSubterms :: TRS -> [Term] -> [Term] -> RWRule -> [([Term], Sub)]
narrowSubterms _ _ [] _ = []
narrowSubterms trs ls (t:rs) rule@(RW l r) = 
    [(ls ++ (ct:rs), cs) | (ct, cs) <- narrowTermForRule False trs rule t]
            ++ narrowSubterms trs (ls ++ [t]) rs rule
               -- subterms are never facts!
