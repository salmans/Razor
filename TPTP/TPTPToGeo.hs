{-| This module facilitates interacting with TPTP problem and solution 
  libraries.
-}

module TPTP.TPTPToGeo (inputsToGeo) where
-- 
import qualified Data.Map as Map
import Data.Maybe
import Control.Exception -- for assert
import Control.Applicative

import Control.Monad
import Control.Monad.Identity
import qualified Control.Monad.State as State

import Data.List
--
import Formula.SyntaxGeo
import qualified Formula.SyntaxFol as Fol

import Utils.GeoUtilities
import qualified Tools.GeoUnification as Unif (lift)
import qualified Utils.GeoUtilities 
import qualified Tools.FolToGeo as F2G

import qualified Codec.TPTP as TP

import Debug.Trace

{-| inputsToGeo converts a list of TPTP inputs (obtained by parsing a TPTP 
  problem file) to a geometric theory, if it is equivalent to a geometric 
  theory. Otherwise, returns Nothing. It also returns a list of constants in 
  the theory, which can be used for constructing a partial model before running 
  the chase on TPTP CNFs. -}
inputsToGeo :: [TP.TPTP_Input] -> Maybe (Theory, [Term])
inputsToGeo inps = do
  (thy, ts, c) <- 
      foldM (\(fs, ts, c) i -> do 
               let cnjr = (TP.unrole.TP.role) i == "conjecture"
               let (f, t) = inputToFol i
               f' <- F2G.formulaToSequent cnjr f
               t'  <- mapM F2G.convertTerm t
               let (f'', t'', c'') = 
                       if cnjr
                       then let (cjrF, cjrT, cjrC) = conjecturize 0 f'
                            in  (cjrF, t' ++ cjrT, cjrC)
                       else (f', t', c)
               return  (fs ++ f'', ts `union` t'', c'')) ([], [], 0) inps'
  return (thy, ts)
    where inps' = filter filterFunc inps
          filterFunc (TP.AFormula _ _ _ _) = True
          filterFunc (TP.Comment _)        = False


conjecturize c seqs = 
    let (result, c') = State.runState (mapM conjecture seqs) c
        (seqs', ts') = unzip result
    in  (concat seqs', nub $ concat ts', c')

{- Converts a TPTP input to a FOL formula, also returns the constants in the
   TPTP formula. The constants will be used to construct a partial model on 
   which the chase is executed when the input is a CNF.-}
inputToFol :: TP.TPTP_Input -> (Fol.Formula, [Fol.Term])
inputToFol = formulaToFol.TP.formula

{- Converts a TPTP formula to a FOL formula, also returns the constants in the
   TPTP formula. The constants will be used to construct a partial model on 
   which the chase is executed when the input is a CNF.-}
formulaToFol :: TP.Formula -> (Fol.Formula, [Fol.Term])
formulaToFol (TP.F f) =
    case runIdentity f of
      (TP.:~:) f'              -> let (f'', ts) = formulaToFol f'
                                  in  (Fol.Not f'', ts)
      TP.Quant TP.All vs f'    -> 
          let vs'        = (\(TP.V v) -> v) <$> vs
              (base, ts) = formulaToFol f'
          in  (foldr (\v f -> Fol.Forall v f) base vs', ts)
      TP.Quant TP.Exists vs f' -> 
          let vs'        = (\(TP.V v) -> v) <$> vs
              (base, ts) = formulaToFol f'
          in  (foldr (\v f -> Fol.Exists v f) base vs', ts)
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
                       in  (Fol.Atm (Fol.R s ts'), cs')
      TP.InfixPred t1 (TP.:=:) t2 -> 
                       let (t1', cs1) = termToFol t1
                           (t2', cs2) = termToFol t2
                       in  (Fol.Atm (Fol.R "=" [t1', t2']), cs1 `union` cs2)
      TP.InfixPred t1 (TP.:!=:) t2 -> 
                       let (t1', cs1) = termToFol t1
                           (t2', cs2) = termToFol t2
                       in  ( Fol.Not $ Fol.Atm (Fol.R "=" [t1', t2'])
                           , cs1 `union` cs2)


termToFol :: TP.Term -> (Fol.Term, [Fol.Term])
termToFol trm@(TP.T t) = 
    case runIdentity t of
      TP.Var (TP.V v) -> (Fol.Var v, [])
      TP.FunApp aw@(TP.AtomicWord f) [] ->
          (Fol.Fn f [], [Fol.Fn f []])
      TP.FunApp aw@(TP.AtomicWord f) ts -> 
          let (ts', cs) = unzip $ map termToFol ts
              cs'       = foldr union [] cs
          in  (Fol.Fn f ts', cs')
      TP.NumberLitTerm d -> (Fol.NumberTerm d, [])
      TP.DistinctObjectTerm s -> (Fol.DistinctTerm s, [])

{-| Converts a set of sequent to a set of conjecture sequents. It uses a 
  counter to create indices for constants. The function also returns a list of 
  terms created during the process. -}
conjecture :: Sequent -> Counter ([Sequent], [Term])
conjecture seq = do
  let vs = freeVars seq
  cs <- mapM (\v -> do { c <- freshSymbol "cjr"; return (Fn c [])}) vs
  let sub  = Map.fromList $ zip vs cs
  let (Sequent bdy hd) = liftTerm (Unif.lift sub) seq
  let hdSequents = (\s -> Sequent s Fls) <$> (splitHead hd)
  let bdySequent = Sequent Tru bdy
  let filtered   = 
          filter (\s -> case s of
                          Sequent Tru Tru -> False
                          Sequent Fls Fls -> False
                          otherwise       -> True) (bdySequent:hdSequents)
  return $  (filtered, cs)
    where splitHead (Or p q)     = splitHead p ++ splitHead q
          splitHead (Exists x p) = [p]  -- Assuming normalized sequents
          splitHead p            = [p]