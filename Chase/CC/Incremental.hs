{- Time-stamp: <2013-07-15 14:33:39 Salman Saghafi>
   Implements an algorithm for computing congruence closure based on
   Bachmair, Tiwari and Vigneron's (BTV) rewriting transformations.
-}

module Chase.CC.Last where

import Control.Applicative

import Data.List
import qualified  Data.Map
import Data.Maybe
import Control.Exception -- for assert
import Debug.Trace

-- Logic modules:
import Chase.Formula.SyntaxGeo
import Chase.Utils.GeoUtilities
import Chase.Tools.GeoUnification


-- Error Messages
err_CCCC_extendTerm_NonGroundRWSys = 
    "err.CC.CC.extendTerm: "
    ++ "The rewrite system cannot have terms that are not ground!"

err_CCCC_extendSubterms_NonGroundRWSys = 
    "err.CC.CC.extendSubterms: "
    ++ "The rewrite system cannot have terms that are not ground!"


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


type TRS = ([RWRule], [Term])

-- A rewrite system representing the state of the computation.
data RWState = RWSt {
    stEquations :: [Equation] 
    , stTRS   :: TRS
    , stRules   :: [RWRule]
    } deriving Show

-- Updates a rewrite system with a list a new equations. It applies
-- cc on the rewrite system and retrurns the new rewrite system.
-- This method returns an initial rewrite system for a given set of
-- equations if the second arguemtn is a pair of empty lists.
-- Inputs: new equations :: [Equation]
--         old rewrite system and its constants :: TRS
-- Output: new rewrite system and the new rules in it :: (TRS, [RWRule])
buildTRS :: [Equation] -> TRS -> (TRS, [RWRule])
buildTRS eqs (rs, cs)  = 
    (trs, exRs)
    where RWSt _ trs exRs = cc $ RWSt eqs (rs,cs) []

-- ADD COMMENT
cc :: RWState -> RWState
cc (RWSt eqs old@(ors, ocs) _) = 
    let (nrs, ncs) = foldr foldFunc ([],[]) eqs
    in (RWSt [] (ors ++ nrs, ocs ++ ncs) nrs)
    where foldFunc trs eq = extend old eq trs

-- ADD COMMENTS
extend :: TRS -> TRS -> Equation -> TRS
extend old new (Eql (Elm "True") term) = extendTrue old new term
extend old new (Eql term (Elm "True")) = extendTrue old new term
extend old@(ors, ocs) new (Eql t1 t2) =
    if t1nf == t2nf
    then t2TRS
    -- else if t1nf `elem` allcs
    --      then (RW t2nf t1nf:t2rs, t2cs)
    --      else if t2nf `elem` allcs
    --           then (RW t1nf t2nf:t2rs, t2cs)
    else (RW t1nf fresh:RW t2nf fresh:t2rs, fresh:t2cs)
    where t1TRS = extendSubterms old new t1
          t2TRS@(t2rs, t2cs) = extendSubterms old t1TRS t2
          allrs = ors ++ t2rs
          allcs = ocs ++ t2cs
          t1nf = normalForm allrs t1
          t2nf = normalForm allrs t2 -- FIX IT
          fresh = freshConstant $ length allcs

-- ADD COMMENT
extendTrue :: TRS -> TRS -> Term -> TRS
extendTrue old@(ors, ocs) new term = 
    if nf == Elm "True"
    then trs
    else (RW nf (Elm "True"):rs, cs)
    where trs@(rs, cs)   = extendSubterms old new term
          allrs = ors ++ rs
          allcs = ocs ++ cs
          nf    = normalForm allrs term
          fresh = freshConstant $ length allcs    


-- ADD COMMENT
extendTerm :: TRS -> TRS -> Term -> TRS
extendTerm old@(ors, ocs) new@(nrs, ncs) term@(Elm e) = 
    case makeRule old new term of
      Nothing            -> new
      Just rule@(RW l r) -> (rule:nrs, r:ncs)
extendTerm old@(ors, ocs) new@(nrs, ncs) term@(Fn f ts) = 
    case makeRule old sub term of
      Nothing            -> sub
      Just rule@(RW l r) -> (rule:srs, r:scs)
    where sub@(srs, scs) = foldr foldFunc new ts
          foldFunc t trs = let (rs, cs) = extendTerm old trs t
                           in  (rs, cs)

-- ADD COMMENT
makeRule :: TRS -> TRS -> Term -> Maybe RWRule
makeRule (ors, ocs) (rs, cs) term = 
    if nf `elem` allcs -- the term denotes in the current TRS
    then Nothing
    else Just $ RW nf fresh
    where allrs = ors ++ rs
          allcs = ocs ++ cs
          fresh = freshConstant (length allcs)
          nf    = normalForm allrs term


--ADD COMMENT
extendSubterms :: TRS -> TRS -> Term -> TRS
extendSubterms _ new (Elm e) = new
extendSubterms old@(ors, ocs) new@(nrs, ncs) t@(Fn f ts) = sub
    where sub@(srs, scs) = foldr foldFunc new ts
          foldFunc t trs@(rs,cs) = let (rs', cs') = extendTerm old trs t
                                   in  (rs ++ rs', cs ++ cs')


--------------------------------
-- Garbage Collection
--------------------------------
-- In the next congruence closure implementation, we should be handle garbage
-- collection within the algorithm as a part of transformations.
garbageCollect :: RWState -> RWState
garbageCollect state@(RWSt eqs (rs, cs) nrs) =
    RWSt eqs (nub rs', nub cs) (nub nrs')
    where (rs', cs') = foldr (\r@(RW x y) (rules,consts) ->
                              if (x `elem` cs) && 
                                     (y `elem` cs) && 
                                     (y /= Elm "True") -- not great!
                              then (rules, consts)
                              else (r:rules, y:consts)) ([],[]) rs
          nrs' = foldr (\r@(RW x y) rules ->
                              if (x `elem` cs) && 
                                     (y `elem` cs) && 
                                     (y /= Elm "True") -- not great!
                              then rules
                              else r:rules) [] nrs


--------------------------------
-- Test Equality
--------------------------------
{- Tries to perform a single rewrite step at the root of t by examining the 
rules (l,r) in order -}
rewrite :: [RWRule] -> Term -> Maybe Term
rewrite [] t = Nothing
rewrite ((RW l r):rest) t = 
    case match [(l, t)] of 
      -- We don't need unification because TRS is ground.
      Nothing -> rewrite rest t
      Just s  -> Just $ liftTerm (lift s) r

{-| Returns the normal form of a term according to a given rewrite system. -}
{- This normalForm function works only if the input rewrite rules are 
reduced. -}
normalForm :: [RWRule] -> Term -> Term
normalForm _ (Var x)    = Var x
normalForm rs (Fn f ts) =
    let u = Fn f (map (normalForm rs) ts)
    in case rewrite rs u of
          Nothing -> u
          Just t  -> normalForm rs t
normalForm rs (Elm e)   = 
    let u = Elm e
    in  case rewrite rs u of
          Nothing -> u
          Just t  -> normalForm rs t


-- Returns true if the two terms have the same normal form in the given
-- rewrite system; otherwise, returns false.
equalTerms :: [RWRule] -> Term -> Term -> Bool
equalTerms trs t1 t2 = (normalForm trs t1) == (normalForm trs t2)
--------------------------------
-- Helpers
--------------------------------

-- Creates a fresh constant
freshConstant :: Int -> Term
freshConstant counter = Elm $ "const" ++ (show counter)


ttruth = Elm "True"
t1 = parseTerm "f(a())"
t2 = parseTerm "a()"
t3 = parseTerm "P(a())"

eq1 = Eql t1 t2
eq2 = Eql t3 ttruth

(trs, _) = buildTRS [eq1] ([],[])
nform = normalForm $ fst trs