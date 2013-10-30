{-| This unit implements a naive congruece closure algorithm compatible with
  the relational algebraic implementation.
-}

module CC.RelAlg (Rule(..),
                  Equation(..),
                  TRS,
--                  buildTRS,
--                  normalForm
)where

import Control.Applicative
import Control.Monad
import qualified Control.Monad.State as State

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace (trace)
import Control.Exception -- for assert
import Data.Tree

-- RelALg
import qualified RelAlg.DB as DB
import Chase.Problem.RelAlg.RelAlg
import Chase.Problem.RelAlg.Operations

-- Logic modules:
import Formula.SyntaxGeo
import Utils.GeoUtilities
import Tools.GeoUnification


{- An equation on two terms (again, only flat terms.) -}
-- data Equation = Eql Term Term 
--                 deriving (Show, Eq)

{-| A rewrite rule is a pair of terms. -}
data Rule = Rule Term Term
            deriving (Ord, Eq, Show)

instance TermBased Rule where
    liftTerm f (Rule t1 t2) = Rule (liftTerm f t1) (liftTerm f t2)
    freeVars (Rule t1 t2)   = union (freeVars t1) (freeVars t2)

{- Term Rewriting System. -}
type TRS = [Rule]

-- flags for operations that still can be applied.
-- flags: sim ext del ori col ded
data Flags = Flag {
      flgSim :: Bool,
      flgExt :: Bool,
      flgDel :: Bool,
      flgOri :: Bool,
      flgCol :: Bool,
      flgDed :: Bool,
      flgCom :: Bool
      } deriving Show

allFlags = Flag True True True True True True True

unsetSim :: Flags -> Flags
unsetSim (Flag _ e d o c de co) = Flag False e d o c de co

unsetExt :: Flags -> Flags
unsetExt (Flag s _ d o c de co) = Flag s False d o c de co

unsetDel :: Flags -> Flags
unsetDel (Flag s e _ o c de co) = Flag s e False o c de co

unsetOri :: Flags -> Flags
unsetOri (Flag s e d _ c de co) = Flag s e d False c de co

unsetCol :: Flags -> Flags
unsetCol (Flag s e d o _ de co) = Flag s e d o False de co

unsetDed :: Flags -> Flags
unsetDed (Flag s e d o c _ co) = Flag s e d o c False co

unsetCom :: Flags -> Flags
unsetCom (Flag s e d o c de _) = Flag s e d o c de False

-- Representing the state of the computation.
data RWState = RWSt {
    stEquations :: [Equation],
    stTRS       :: TRS,
    stTables    :: Tables,
    stDeltas    :: Tables,
    stFlags     :: Flags
    } deriving Show


-- Computes a congruence closure for a set of equations.
-- The order of applying the operations should not should matter.
-- Returns a list of rules that it introduces to the input rewrite
-- system.
cc :: RWState -> Counter RWState
cc st
   | flgSim flgs = sim st >>= cc
   | flgExt flgs = ext st >>= cc
   | flgDel flgs = del st >>= cc
   | flgOri flgs = ori st >>= cc
   | flgCol flgs = col st >>= cc
   | flgDed flgs = ded st >>= cc
   | flgCom flgs = com st >>= cc
   | otherwise   = return st
   where flgs = stFlags st
-- buildTRS :: [Equation] -> (TRS, [Term]) -> ((TRS, [Term]), [RWRule])
-- buildTRS eqs (sys, cs)  = 
--     ((stRules new, stConstants new), pl)
--     where (new,pl) = cc (RWSt cs eqs sys allFlags, [])

--------------------------------
-- Simplification
-- (K,E[t],R∪{t → c}) => (K, E[c], R ∪ {t → c})
--------------------------------
sim :: RWState -> Counter RWState
sim (RWSt eqs trs tbls delts flgs) = 
    return $ RWSt eqs' trs tbls delts (unsetSim flgs)
    where eqs' = liftTerm (normalForm trs) <$> eqs

--------------------------------
-- Extension
-- (K,E[t],R) => (K ∪ {c}, E[c], R ∪ {t → c})
--------------------------------
ext :: RWState -> Counter RWState
ext (RWSt eqs trs tbls delts flgs) = do
  (delts', eqs', trs') <- foldM initEquation (tbls,[], trs) eqs
  let tbls'            =  mergeSets tbls delts'
  return (RWSt eqs' trs' tbls' delts' (unsetExt flgs))

initEquation :: (Tables, [Equation], TRS) -> Equation -> 
                Counter (Tables, [Equation], TRS)
initEquation (rs, eqs, rls) (Equ t1 t2) = do 
  (r1, c1) <- initConstant rs t1
  let rs'  =  mergeSetsWithKey unionFunc rs r1
  (r2, c2) <- initConstant rs' t2
  let rls' =  (ruleOf t1 c1) `union` (ruleOf t2 c2) 
  return (mergeSetsWithKey unionFunc rs' r2
         , eqs ++ [Equ c1 c2]
         , rls `union` rls')
  where unionFunc k  = if k == DomTable then unionSets else const
        -- Maintain the elements of the domain table but replace the elements
        -- of FunTables in order to assige every constant to only one value.
        ruleOf t1 c1 = case t1 of
                         Fn _ []   -> [Rule t1 c1]
                         otherwise -> []
                         -- Only extend constants. Relational facts are inserted 
                         -- as records by orientation.
                         
--------------------------------
-- Deletion
-- (K, E ∪ {t ≈ t}, R) => (K, E, R)
--------------------------------
del :: RWState -> Counter RWState
del (RWSt eqs trs tbls delts flgs) =
    return $ RWSt eqs' trs tbls delts flgs
    where eqs' = filter (\(Equ t1 t2) -> t1 /= t2) eqs
--------------------------------
-- Orientation
-- (K ∪ {c}, E ∪ {t ≈ c}, R) => (K ∪ {c}, E, R ∪ {t → c})
--------------------------------
ori :: RWState -> Counter RWState
ori (RWSt eqs trs tbls delts flgs) = do
  (tbls', trs') <- foldM orientEquation (tbls, trs) eqs
  return (RWSt eqs trs' tbls' delts flgs)

orientEquation :: (Tables, TRS) -> Equation -> Counter (Tables, TRS)
orientEquation (tbls, trs) (Equ (Fn f ts) (Elm "True")) = do
    let newRec = Map.singleton (RelTable f) $ DB.Set [ts]
    return (mergeSets tbls newRec, trs)
orientEquation (tbls, trs) (Equ (Elm "True") t) = 
    orientEquation (tbls, trs) (Equ t (Elm "True")) -- orient equation
orientEquation (tbls, trs) (Equ t1@(Elm c1) t2@(Elm c2)) = do
    let rl = if t1 >= t2 then (Rule t1 t2) else (Rule t2 t1)
    return (tbls, nub (rl:trs))
orientEquation _ _ = error $ "CC.RelAlg.orientEquation: invalid equation"
--------------------------------
-- Collapse
-- (K, E, R ∪ {s[c] → c′, c → d}) => (K, E, R ∪ {s[d] → c′, c → d})
--------------------------------
col :: RWState -> Counter RWState
col (RWSt eqs trs tbls delts flgs) = undefined
  

--------------------------------
-- Deduction
-- (K, E, R ∪ {t → c, t → d}) => (K, E ∪ {c ≈ d}, R ∪ {t → d})
--------------------------------
-- Deduces a new equation in a given state
ded :: RWState -> Counter RWState
ded = undefined
--------------------------------
-- Composition
-- (K, E, R ∪ {t → c, c → d}) => (K, E, R ∪ {t → d, c → d})
--------------------------------
com :: RWState -> Counter RWState
com = undefined
--------------------------------
-- Test Equality
--------------------------------
{- Tries to perform a single rewrite step at the root of t by examining the 
rules (l,r) in order -}
rewrite :: TRS -> Term -> Maybe Term
rewrite [] t = Nothing
rewrite ((Rule l r):rest) t = 
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

-- --------------------------------
-- -- Helpers
-- --------------------------------
-- -- Creates a fresh constant
-- freshConstant :: Int -> Term
-- freshConstant counter = Elm $ "const" ++ (show counter)
-- --------------------------------
-- -- Tests
-- --------------------------------
-- testEqual :: TRS -> String -> String -> Bool
-- testEqual rs str1 str2 = equalTerms rs (parseTerm str1) (parseTerm str2) 

-- makeEql :: String -> String -> Equation
-- makeEql str1 str2 = Eql (parseTerm str1) (parseTerm str2)

-- makeRW :: String -> String -> RWRule
-- makeRW str1 str2 = RW (parseTerm str1) (parseTerm str2)


-- TEST DATA
eq1 = Equ (Fn "a" []) (Fn "b" []) -- a = b
eq2 = Equ (Fn "a" []) (Elm "c0")  -- a = c0
eq3 = Equ (Elm "c1") (Elm "c2")

rl1 = Rule (Elm "c1") (Elm "c2")
rl2 = Rule (Elm "c2") (Elm "c0")
rl3 = Rule (Elm "c0") (Elm "c3")

st1 = RWSt [eq1, eq2, eq3] [rl1, rl2, rl3] emptyTables emptyTables allFlags

run st = State.runState (ext st) 4