{- Time-stamp: <2013-02-11 23:57:15 Salman Saghafi>
   Implements an algorithm for computing congruence closure based on
   Bachmair, Tiwari and Vigneron's (BTV) rewriting transformations.
-}

module CC.Naive (
                 RWRule(..),
                 Equation(..),
                 TRS,
                 buildTRS,
                 normalForm
)where

import Control.Applicative

import Text.ParserCombinators.Parsec hiding ( (<|>) )
import Text.Parsec.Token ( TokenParser )
import qualified Text.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language ( haskellStyle )
import qualified Text.ParserCombinators.Parsec.Expr as Expr

import Data.List
import qualified Data.Map
import Data.Maybe
import Debug.Trace (trace)
import Control.Exception -- for assert
import Data.Tree


-- Logic modules:
import Formula.SyntaxGeo
import Utils.GeoUtilities
import Tools.GeoUnification


-- A rewrite rule in the form of a D-rule or a C-rule
data RWRule = RW Term Term
              deriving (Show, Eq, Ord)

-- An equation on two terms
data Equation = Eql Term Term 
                deriving (Show, Eq)


-- Term Rewriting System
type TRS = [RWRule]

-- flags for operations that still can be applied.
-- flags: sim ext del ori col ded
data RWFlags = Flg {
      flgSim :: Bool,
      flgExt :: Bool,
      flgDel :: Bool,
      flgOri :: Bool,
      flgCol :: Bool,
      flgDed :: Bool,
      flgCom :: Bool
      } deriving Show

allFlags = Flg True True True True True True True

unsetSim :: RWFlags -> RWFlags
unsetSim (Flg _ e d o c de co) = Flg False e d o c de co

unsetExt :: RWFlags -> RWFlags
unsetExt (Flg s _ d o c de co) = Flg s False d o c de co

unsetDel :: RWFlags -> RWFlags
unsetDel (Flg s e _ o c de co) = Flg s e False o c de co

unsetOri :: RWFlags -> RWFlags
unsetOri (Flg s e d _ c de co) = Flg s e d False c de co

unsetCol :: RWFlags -> RWFlags
unsetCol (Flg s e d o _ de co) = Flg s e d o False de co

unsetDed :: RWFlags -> RWFlags
unsetDed (Flg s e d o c _ co) = Flg s e d o c False co

unsetCom :: RWFlags -> RWFlags
unsetCom (Flg s e d o c de _) = Flg s e d o c de False

-- Representing the state of the computation.
data RWState = RWSt {
    stConstants :: [Term],
    stEquations :: [Equation], 
    stRules :: TRS,
    stFlags :: RWFlags
    } deriving Show


-- Computes a congruence closure for a set of equations.
-- The order of applying the operations should not should matter.
-- Returns a list of rules that it introduces to the input rewrite
-- system.
cc :: (RWState, [RWRule]) -> (RWState, [RWRule])
cc (st@(RWSt _ _ _ flgs), rs)
   | flgSim flgs = cc (sim (st, rs))
   | flgExt flgs = cc (ext (st, rs))
   | flgDel flgs = cc (del (st, rs))
   | flgOri flgs = cc (ori (st, rs))
   | flgCol flgs = cc (col (st, rs))
   | flgDed flgs = cc (ded (st, rs))
   | flgCom flgs = cc (com (st, rs))
   | otherwise = (st, rs)

-- Updates a rewrite system with a list a new equations. It applies
-- cc on the rewrite system and returns the new rewrite system.
-- This method returns an initial rewrite system for a given set of
-- equations if the second arguemtn is a pair of empty lists.
-- Inputs: new equations :: [Equation]
--         old rewrite system and its constants :: (TRS, [Term])
-- Output: new rewrite system and its constants :: (TRS, [Term])
buildTRS :: [Equation] -> (TRS, [Term]) -> ((TRS, [Term]), [RWRule])
buildTRS eqs (sys, cs)  = 
    ((stRules new, stConstants new), pl)
    where (new,pl) = cc (RWSt cs eqs sys allFlags, [])

drawTRS :: TRS -> [Tree Term] -> [Tree Term]
drawTRS (r:rs) trees = undefined


addToTree :: RWRule -> Tree Term -> Maybe (Tree Term)
addToTree r@(RW t1 t2) (Node n ts) =
    if t1 == n
    then Just (Node n ((Node t2 []):ts))
    else (if null ts
          then Nothing
          else Just (Node n ts'))
    where ts' = map fromJust $ filter isJust $ map (addToTree r) ts

--------------------------------
-- Simplification
--------------------------------
-- Applies simplification on the first equation that can be simplified.
-- If there is no such equation, unsets the simplification flag.
sim :: (RWState, [RWRule]) -> (RWState, [RWRule])
sim (RWSt cs [] rs flgs, newRules) = (RWSt cs [] rs (unsetSim flgs), newRules)
sim (RWSt cs (eq:eqs) rs flgs, newRules) =
    (if eq == eq'
    then (RWSt cs (eq:eqs') rs flgs', newRules) -- cannot simplify this term
    else (RWSt cs (eq':eqs) rs allFlags, newRules))
    where eq' = foldr (\r e -> (onEquation (simTerm r)) e) eq rs
          (RWSt _ eqs' _ flgs', _) = sim (RWSt cs eqs rs flgs, newRules)

-- Simplieifes a term w.r.t a rewrite rule
simTerm :: RWRule -> Term -> Term
simTerm _ (Var _) = error "simTerm: the input term must be ground!"
simTerm r@(RW t1 t2) t@(Fn f ts)
        | t == t1 = t2
        | otherwise = Fn f (map (simTerm r) ts)
simTerm r@(RW t1 t2) t@(Elm _)
        | t == t1 = t2
        | otherwise = t

--------------------------------
-- Extension
--------------------------------
-- Applies one extension step on an equation. It tries to extend the term on
-- left first; if it cannot find any extension for the term on left, extends
-- the term on right. Also, within each term, it extends its subterms from
-- left to right and returns if an extension is applicable; if none of the
-- subterms can be extended, it tries to extend the term itself.
-- If it can find an equation to extend, it extends the equation and returns
-- the results; otherwise, unsets the extension flag.
ext :: (RWState, [RWRule]) -> (RWState, [RWRule])
ext (RWSt cs [] rs flgs, newRules) = (RWSt cs [] rs (unsetExt flgs), newRules)
ext (RWSt cs (eq@(Eql t1 t2):eqs) rs flgs, newRules) = 
    case (ext_t1, ext_t2) of
      (Nothing, Nothing) -> (RWSt cs' (eq:eqs') rs' flgs', newRules')
      (Just (t1', c' ,rules'), _) -> (RWSt (c':cs) ((Eql t1' t2):eqs) 
                                           (rs ++ rules') allFlags, 
                                           newRules' ++ rules')
      (Nothing, Just (t2',c' ,rules')) -> (RWSt (c':cs) ((Eql t1 t2'):eqs) 
                                                (rs ++ rules') allFlags, 
                                                newRules' ++ rules')
    where (RWSt cs' eqs' rs' flgs', newRules') = 
              ext (RWSt cs eqs rs flgs, newRules)
          ext_t1 = extTerm cs t1 rs
          ext_t2 = extTerm cs t2 rs


extTerm :: [Term] -> Term -> TRS -> Maybe (Term, Term, TRS)
extTerm _ (Var _) _ = error "extTerm: the input term must be ground!"
extTerm consts elm@(Elm sym) rules =
    if elm `elem` consts
    then Nothing
    else case sym of
           "True"    -> Just (elm, elm, []) -- Elm "True" is a special case
           otheriwse -> Just (new, new, [RW elm new])
    where new = freshConstant (length consts)
extTerm consts term@(Fn f []) rules = Just (new, new, [RW term new])
    where new = freshConstant (length consts)
extTerm consts term@(Fn f terms) rules = 
    case extSubterms rules consts terms of
      Nothing -> Just (new, new, [RW term new])
      Just (terms', sym', rules') -> Just (Fn f terms', sym', rules')
    where new = freshConstant $ length consts

-- This function extends the firs term that can be extended and 
-- keeps the rest unchanged. It returns the new list of terms,
-- the new symbol replacing the extended term, and the new TRS
-- if such extension exists. Otherwise, returns Nothing.
extSubterms :: TRS -> [Term] -> [Term] -> Maybe ([Term], Term, TRS)
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
                     True -> (True, t:ts, s, rs))) (False,[], Elm "c",rules) terms

--------------------------------
-- Deletion
--------------------------------
del :: (RWState, [RWRule]) -> (RWState, [RWRule])
del (RWSt cs [] rs flgs, newRules) = (RWSt cs [] rs (unsetDel flgs), newRules)
del (RWSt cs (eq@(Eql t1 t2):eqs) rs flgs, newRules) =
    if t1 == t2
    then (RWSt cs (eqs) rs flgs, newRules) -- deletion does not cause new transformations
    else (RWSt cs (eq:eqs') rs flgs', newRules)
    where (RWSt _ eqs' _ flgs', _) = del (RWSt cs eqs rs flgs, newRules)
--------------------------------
-- Orientation
--------------------------------
-- Creates a rewriting rule corresponding to an equation based on an orientation 
-- transformation.
-- This function transforms the first equation in the state.
ori :: (RWState, [RWRule]) -> (RWState, [RWRule])
ori (RWSt cs [] rs flgs, newRules) = (RWSt cs [] rs (unsetOri flgs), newRules)
ori (RWSt cs (eq@(Eql t1 t2):eqs) rs flgs, newRules)
    | t1 `elem` cs && t2 `elem` cs =
        if t1 > t2 || t2 == Elm "True" -- Elm "True" is smaller than everything
        then (RWSt cs eqs ((RW t1 t2):rs) allFlags, (RW t1 t2):newRules')
        else (RWSt cs eqs ((RW t2 t1):rs) allFlags, (RW t2 t1):newRules')
    | t1 `elem` cs = (RWSt cs eqs (RW t2 t1:rs) allFlags, (RW t2 t1):newRules')
    | t2 `elem` cs = (RWSt cs eqs (RW t1 t2:rs) allFlags, (RW t1 t2):newRules')
    | otherwise = (RWSt cs (eq:eqs') rs' flgs', newRules')
    where (RWSt _ eqs' rs' flgs', newRules') = 
              ori (RWSt cs eqs rs flgs, newRules)

--------------------------------
-- Collapse
--------------------------------
-- Collapses a rewrite rule according to another rule.
-- This function takes the first applicable rewrite rule as the reference rule
-- and collapses the other rules accordingly. If it cannot find such rule,
-- unsets the collapse flag.
-- Note that it only collapses the left of the rule.
col :: (RWState, [RWRule]) -> (RWState, [RWRule])
col (RWSt cs eqs rs flgs, newRules) =
    case col' cs rs [] newRules of
      (Nothing, _) -> (RWSt cs eqs rs (unsetCol flgs), newRules)
      (Just rs', newRules') -> (RWSt cs eqs rs' allFlags, newRules')


-- A helper for collapse: it divides the rules into two lists: processed and
-- unprocessed and always uses the first unprocessed rule as the reference
-- rule for the collapse transformation. It returns Nothing if none of the
-- rules could cause a collapse of the other rules. Otherwise, returns the
-- collapsed rules.
col' :: [Term] -> TRS -> TRS -> [RWRule] -> (Maybe TRS, [RWRule])
col' _ [] _ newRules = (Nothing, newRules)
col' consts (rule:rs) rs' newRules =
    case colHelper consts rule (rs ++ rs') newRules of
      (Nothing, _) -> col' consts rs (rule:rs') newRules
      (Just collapsed, newRules') -> (Just (rule:collapsed), rule:newRules')


-- A helper for col'
colHelper :: [Term] -> RWRule -> TRS -> [RWRule]-> (Maybe TRS, [RWRule])
colHelper _ rule [] newRules = (Nothing, newRules)
colHelper consts rule@(RW rt1 rt2) (r@(RW t1 t2):rs) newRules =
    if  (greater t2 t1') || (t1 == t1') -- it has to respect term ordering
    then ((r:) <$> rest, newRules') -- doesn't collapse this rule
    else (((r':) <$> rest) <|> Just (r':rs), r':delete r newRules')
    where r'@(RW t1' _) = RW (colTerm rule t1) t2
          (rest, newRules') = colHelper consts rule rs newRules
          greater x y = 
              case (x `elem` consts, y `elem` consts) of
                (True, True) -> x >= y
                (False, False) -> x >= y
                (False, True) -> True
                (True, False) -> False

-- Collapses a term w.r.t a rewrite rule (happens to be similar to simTerm)
colTerm :: RWRule -> Term -> Term
colTerm _ (Var _) = error "colTerm: the input term must be ground!"
colTerm r@(RW t1 t2) t@(Fn f ts)
        | t == t1 = t2 
        | otherwise = Fn f (map (colTerm r) ts)
colTerm r@(RW t1 t2) t@(Elm _)
        | t == t1 = t2
        | otherwise = t

--------------------------------
-- Deduction
--------------------------------
-- Deduces a new equation in a given state
ded :: (RWState, [RWRule]) -> (RWState, [RWRule])
ded (RWSt cs eqs [] flgs, newRules) = (RWSt cs eqs [] (unsetDed flgs), newRules)
ded (RWSt cs eqs (r@(RW t1 t2):rs) flgs, newRules) =
    case r' of
      Nothing -> (RWSt cs restEqs (r:restRules) flgs', newRules')
      Just (RW t1' t2') -> (RWSt cs ((Eql t2 t2'):eqs) rs allFlags, 
                                 delete r newRules')
    where r' = find (\(RW x y) -> x == t1) rs 
          (RWSt _ restEqs restRules flgs', newRules') = 
              ded (RWSt cs eqs rs flgs, newRules)

--------------------------------
-- Composition
--------------------------------
-- Composes two rules in the TRS
com :: (RWState, [RWRule]) -> (RWState, [RWRule])
com (state@(RWSt cs eqs rules flgs), newRules) = comHelper rules state newRules

comHelper :: [RWRule] -> RWState -> [RWRule] -> (RWState, [RWRule])
comHelper _ (RWSt cs eqs [] flgs) newRules = (RWSt cs eqs [] (unsetCom flgs), newRules)
comHelper allRules (RWSt cs eqs (r@(RW t1 t2):rs) flgs) newRules =
    (case r' of
      Nothing -> (RWSt cs restEqs (r:restRules) flgs', newRules')
      Just (RW t1' t2') -> (RWSt cs eqs ((RW t1 t2'):rs) allFlags,
                            union [RW t1 t2'] (delete (RW t1 t2) newRules')))
    where r' = find (\(RW x y) -> x == t2) allRules

          (RWSt _ restEqs restRules flgs', newRules') = 
              comHelper allRules (RWSt cs eqs rs flgs) newRules

--------------------------------
-- Flattens the terms of a set of equations.
-- Inputs:
--     A counter that keeps track of the constants that have been added so far.
--     A set of equations (left) to keep track of equations whose left side
--   has been flattened so far.
--     A set of equations (right) that keeps track of equations whose left term
--   has been flattened but the right term has not.
-- Output:
--     Last counter representing the number of constants in the form of "c(n)"
--   that have been created by this procedure.
--     A new set of equations with flattened terms.
--
-- Starting State: 0 equations []
-- flatten :: Int -> [Equation] -> [Equation] -> (Int, [Equation])
-- flatten counter [] [] = (counter, []) -- We are done
-- flatten counter ((Eql (Fn f ts) t2):rest) right = -- process left
--     (restCounter, restEqs)
--     where eq = (Eql (Fn f ts') t2)
--           newEqs = zipWith (\x y -> Eql x y) ts' ts
--           ts' = map (\x -> Fn x []) newConstants
--           newConstants = map (\x -> freshConstant (counter + x)) [1..arity]
--           arity = length ts
--           (restCounter, restEqs) = flatten (counter + arity) (rest ++ newEqs) (eq:right)
-- flatten counter left right@((Eql t1 (Fn f ts)):rest) = -- process right
--     (restCounter, eq:restEqs)
--     where eq = (Eql t1 (Fn f ts'))
--           newEqs = zipWith (\x y -> Eql x y) ts' ts
--           ts' = map (\x -> Fn x []) newConstants
--           newConstants = map (\x -> freshConstant (counter + x)) [1..arity]
--           arity = length ts
--           (restCounter, restEqs) = flatten (counter + arity) left (rest ++ newEqs)

-- applyFlatten :: [(String, String)] -> [Equation]
-- applyFlatten pairs = snd (flatten 0 eqs [])
--     where eqs = map (\(x, y) -> Eql (parseTerm x) (parseTerm y)) pairs

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
-- Applies a function on an equation
onEquation :: (Term -> Term) -> Equation -> Equation
onEquation f (Eql t1 t2) = Eql (f t1) (f t2)

-- Applies a function on a rewrite rule
onRWRule :: (Term -> Term) -> RWRule -> RWRule
onRWRule f (RW t1 t2) = RW (f t1) (f t2)

-- Creates a fresh constant
freshConstant :: Int -> Term
freshConstant counter = Elm $ "const" ++ (show counter)
--------------------------------
-- Tests
--------------------------------
testEqual :: TRS -> String -> String -> Bool
testEqual rs str1 str2 = equalTerms rs (parseTerm str1) (parseTerm str2) 

makeEql :: String -> String -> Equation
makeEql str1 str2 = Eql (parseTerm str1) (parseTerm str2)

makeRW :: String -> String -> RWRule
makeRW str1 str2 = RW (parseTerm str1) (parseTerm str2)
