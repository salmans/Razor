{-| This module facilitates interacting with TPTP problem and solution 
  libraries.
-}

module Syntax.ITPTP (parse) where

-- Standard
import Data.List
import qualified Data.Map as Map
import Data.Maybe

-- Control
import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import qualified Control.Monad.State as State

-- Syntax
import Syntax.Term (Term (..), Variable (..), Element (..), Constant (..)
                   , TermBased (..))
import Syntax.Geometric
import Syntax.IGeometricUtils
import qualified Syntax.FirstOrder as Fol

-- Common
import Common.Basic (freshSymbol)

-- Tools
import Tools.Counter
import Tools.Trace
import qualified Tools.FolToGeo as F2G

-- TPTP
import qualified Codec.TPTP as TP


{-| Parses the input String to a list of TPTP inputs and converts the resulting
    list to a geometric theory if possible. The function also returns a list of
    constants in the theory, which may be used for constructing an initial 
    partial model for CNF formulas. -}
parse :: String -> Maybe (Theory, [Constant], [String])
parse  = inputsToGeo . TP.parse

{- inputsToGeo converts a list of TPTP inputs (obtained by parsing a TPTP 
  problem file) to a geometric theory, if it is equivalent to a geometric 
  theory. Otherwise, returns Nothing. It also returns a list of constants in 
  the theory, which can be used for constructing an initial partial model for
  TPTP CNF formulas. -}
inputsToGeo :: [TP.TPTP_Input] -> Maybe (Theory, [Constant], [String])
inputsToGeo inps = do
  (thy, ts, c) <- 
      foldM (\(fs, ts, c) i -> do 
               let cnjr = (TP.unrole.TP.role) i == "conjecture"
               let (f, t) = inputToFol i
               f' <- F2G.formulaToTheory cnjr f
               let (f'', t'', c'') = 
                       if cnjr
                       then let (cjrF, cjrT, cjrC) = conjecturize 0 f'
                            in  (cjrF, t ++ cjrT, cjrC)
                       else (f', t, c)
               return  (fs ++ f'', ts `union` t'', c'')) ([], [], 0) formulas
  return (thy, ts, includes)
    where 
      formulas = filter filterFormula inps
      filterFormula (TP.AFormula _ _ _ _) = True
      filterFormula (TP.Comment _)        = False
      filterFormula (TP.Include _ _) = False
      includes = do
        let is = filter filterInclude inps
        let paths = map (\(TP.Include path _)->path) is
        paths
      filterInclude (TP.AFormula _ _ _ _) = False
      filterInclude (TP.Comment _)        = False
      filterInclude (TP.Include _ _) = True


conjecturize c seqs = 
    let (result, c') = State.runState (mapM conjecture seqs) c
        (seqs', ts') = unzip result
    in  (concat seqs', nub $ concat ts', c')

{- Converts a TPTP input to a FOL formula, also returns the constants in the
   TPTP formula. -}
inputToFol :: TP.TPTP_Input -> (Fol.Formula, [Constant])
inputToFol = formulaToFol.TP.formula

{- Converts a TPTP formula to a FOL formula, also returns the constants in the
   TPTP formula. -}
formulaToFol :: TP.Formula -> (Fol.Formula, [Constant])
formulaToFol (TP.F f) =
    case runIdentity f of
      (TP.:~:) f'              -> let (f'', ts) = formulaToFol f'
                                  in  (Fol.Not f'', ts)
      TP.Quant TP.All vs f'    -> 
          let vs'        = (\(TP.V v) -> v) <$> vs
              (base, ts) = formulaToFol f'
          in  (foldr (\v f -> Fol.Forall (Variable v) f) base vs', ts)
      TP.Quant TP.Exists vs f' -> 
          let vs'        = (\(TP.V v) -> v) <$> vs
              (base, ts) = formulaToFol f'
          in  (foldr (\v f -> Fol.Exists Nothing (Variable v) f) base vs', ts)
      TP.BinOp p (TP.:|:) q   -> let (p', pts) = formulaToFol p
                                     (q', qts) = formulaToFol q                                                 
                                 in  (Fol.Or p' q', pts `union` qts)
      TP.BinOp p (TP.:&:) q   -> let (p', pts) = formulaToFol p
                                     (q', qts) = formulaToFol q                                                 
                                 in  (Fol.And p' q', pts `union` qts)
      TP.BinOp p (TP.:=>:) q  -> let (p', pts) = formulaToFol p
                                     (q', qts) = formulaToFol q                                                 
                                 in  (Fol.Imp p' q', pts `union` qts)
      TP.BinOp p (TP.:<=>:) q -> let (p', pts) = formulaToFol p
                                     (q', qts) = formulaToFol q                                                 
                                 in  (Fol.Iff p' q', pts `union` qts)
      TP.PredApp (TP.AtomicWord "$true")  [] -> (Fol.Tru, [])
      TP.PredApp (TP.AtomicWord "$false") [] -> (Fol.Fls, [])
      TP.PredApp (TP.AtomicWord s) ts -> 
                       let (ts', cs) = unzip $ map termToFol ts
                           cs'       = foldr union [] cs
                       in  (Fol.Atm (Fol.Rel s ts'), cs')
      TP.InfixPred t1 (TP.:=:) t2 -> 
                       let (t1', cs1) = termToFol t1
                           (t2', cs2) = termToFol t2
                       in  (Fol.Atm (Fol.Rel "=" [t1', t2']), cs1 `union` cs2)
      TP.InfixPred t1 (TP.:!=:) t2 -> 
                       let (t1', cs1) = termToFol t1
                           (t2', cs2) = termToFol t2
                       in  ( Fol.Not $ Fol.Atm (Fol.Rel "=" [t1', t2'])
                           , cs1 `union` cs2)


termToFol :: TP.Term -> (Term, [Constant])
termToFol trm@(TP.T t) = 
    case runIdentity t of
      TP.Var (TP.V v) -> (Var $ Variable v, [])
      TP.FunApp aw@(TP.AtomicWord f) [] ->
          (Cons $ Constant f, [Constant f])
      TP.FunApp aw@(TP.AtomicWord f) ts -> 
          let (ts', cs) = unzip $ map termToFol ts
              cs'       = foldr union [] cs
          in  (Fn f ts', cs')
      TP.NumberLitTerm d -> (NumberTerm d, [])
      TP.DistinctObjectTerm s -> (DistinctTerm s, [])

{-| Converts a set of sequent to a set of conjecture sequents. It uses a 
  counter to create indices for the new constants. The function also returns a 
  list of terms created during the process. -}
conjecture :: Sequent -> Counter ([Sequent], [Constant])
conjecture seq = do
  let vs = freeVars seq
  cs <- mapM (\v -> do { c <- freshSymbol "cjr"; return (Constant c)}) vs
  let sub  = Map.fromList $ zip vs (Cons <$> cs)
  let (Sequent bdy hd) = substitute sub seq
  let hdSequents = (\s -> Sequent s Fls) <$> (splitHead hd)
  let bdySequent = Sequent Tru bdy
  let filtered   = 
          filter (\s -> case s of
                          Sequent Tru Tru -> False
                          Sequent Fls Fls -> False
                          otherwise       -> True) (bdySequent:hdSequents)
  return $  (filtered, cs)
    where splitHead (Or p q)       = splitHead p ++ splitHead q
          splitHead (Exists _ x p) = [p]  -- Assuming normalized sequents
          splitHead p              = [p]