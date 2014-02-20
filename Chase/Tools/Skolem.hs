-- 
module Chase.Tools.Skolem where
-- 
import Prelude 
import qualified List 
import qualified Data.Map as Map
import SyntaxFol
import qualified FolUtilities as U

-- %%%%%%%%%%%%%%%%%%%%%
-- %%% Simplification
-- 
simplify :: Formula Fol -> Formula Fol
simplify (Not p) = simplify1 $ Not $ simplify p
simplify (And p q) = simplify1 (And (simplify p) (simplify q))
simplify (Or p q) = simplify1 (Or (simplify p) (simplify q))
simplify (Imp p q) = simplify1 (Imp (simplify p) (simplify q))
simplify (Iff p q) = simplify1 (Iff (simplify p) (simplify q))
simplify (Forall x p) = simplify1 $ Forall x (simplify p)
simplify (Exists x p) = simplify1 $ Exists x (simplify p)
simplify fm = fm
-- 
simplify1 :: Formula Fol -> Formula Fol
simplify1 fm = 
       case fm of
              Forall x p -> if List.elem x (U.fv p) then fm else p
              Exists x p -> if List.elem x (U.fv p) then fm else p
              Not Bot -> Top
              Not Top -> Bot
              And Bot _ -> Bot
              And _ Bot -> Bot
              And Top q -> q
              And p Top -> p
              Or Bot q -> q
              Or p Bot -> p
              Or Top _ -> Top
              Or _ Top -> Top
              Imp Bot _ -> Top
              Imp Top q -> q
              Imp _ Top -> Top
              Imp p Bot -> Not p
              Iff Top q -> q
              Iff p Top -> p
              Iff Bot q -> Not q
              Iff p Bot -> Not p
              _ -> fm              
-- 
-- %%%%%%%%%%%%%%%%%%%%%%%%
-- %%% Negation Normal Form
-- 
nnf :: Formula a -> Formula a
nnf (And p q) = And (nnf p) (nnf q)
nnf (Or p q) = Or (nnf p) (nnf q)
nnf (Imp p q) = Or (nnf (Not p)) (nnf q)
nnf (Iff p q) = Or (And (nnf p) (nnf q)) (And (nnf(Not p)) (nnf(Not q)))
nnf (Not(Not p)) = nnf p
nnf (Not(And p q)) = Or (nnf(Not p)) (nnf(Not q))
nnf (Not(Or p q)) = And (nnf(Not p)) (nnf(Not q))
nnf (Not(Imp p q)) = And (nnf p) (nnf(Not q))
nnf (Not(Iff p q)) = Or (And (nnf p) (nnf(Not q))) (And (nnf(Not p)) (nnf q))
nnf (Forall x p) = Forall x (nnf p)
nnf (Exists x p) = Exists x (nnf p)
nnf (Not (Forall x p)) = Exists x (nnf (Not p))
nnf (Not (Exists x p)) = Forall x (nnf (Not p))
nnf fm = fm
-- 
-- nnf $ read "(forall x. P(x)) => ((exists y. Q(y)) <=> exists z. P(z) & Q(z))"
-- 
-- %%%%%%%%%%%%%%%%%%%%%%%
-- %%% Prenex Normal Form
-- 
pnf :: Formula Fol -> Formula Fol
pnf = prenex . nnf . simplify
-- 
prenex :: Formula Fol -> Formula Fol
prenex (Forall x p) = Forall x (prenex p)
prenex (Exists x p) = Exists x (prenex p)
prenex (And p q) = pullquants (And (prenex p) (prenex q))
prenex (Or p q) = pullquants (Or (prenex p)  (prenex q))
prenex fm = fm
-- 
pullquants :: Formula Fol -> Formula Fol
pullquants fm = case fm of
 And (Forall x p) (Forall y q) -> 
   pullq (True,True) fm Forall And x y p q
 Or (Exists x p) (Exists y q) -> 
   pullq (True,True) fm Exists Or x y p q
 And (Forall x p) q -> pullq (True,False) fm Forall And x x p q
 And p (Forall y q) -> pullq (False,True) fm Forall And y y p q
 Or (Forall x p) q ->  pullq (True,False) fm Forall Or x x p q
 Or p (Forall y q) ->  pullq (False,True) fm Forall Or y y p q
 And (Exists x p) q -> pullq (True,False) fm Exists And x x p q
 And p (Exists y q) -> pullq (False,True) fm Exists And y y p q
 Or (Exists x p) q ->  pullq (True,False) fm Exists Or x x p q
 Or p (Exists y q) ->  pullq (False,True) fm Exists Or y y p q
 _ -> fm
-- 
pullq :: (Bool, Bool) -> Formula Fol 
     -> (Var -> Formula Fol -> Formula Fol) 
     -> (Formula Fol -> Formula Fol -> Formula Fol) -> Var -> Var
     -> Formula Fol -> Formula Fol -> Formula Fol
pullq (l,r) fm quant op x y p q =
 let z = U.variant x (U.fv fm) 
     p' = if l then U.apply (Map.singleton x (Var z)) p else p
     q' = if r then U.apply  (Map.singleton y (Var z)) q else q in
 quant z (pullquants(op p' q'))
-- 


-- %%%%%%%%%%%%%%%%%
-- %%% Skolemization
-- 

f = specialize . pnf . askolemize

-- poor names in Harrison's book: avoid confusion
skolemize :: Formula Fol -> Formula Fol 
skolemize = -- specialize . pnf . askolemize
  error "in module Skolem: use stripSkolemize if you want no quantifiers"

-- just another, better, name for skolemization
-- skolemForm fmla is a universal formula equi-satisfiable with fmla
skolemForm :: Formula Fol -> Formula Fol
skolemForm = askolemize

-- this one leaves the universal quantifiers in place
askolemize :: Formula Fol -> Formula Fol 
askolemize fm = fst ((skolem $ nnf $ simplify fm) (map fst (U.functions fm)))

--
-- skolemize and then remove the universal quantifiers
stripSkolemize :: Formula Fol -> Formula Fol 
stripSkolemize = specialize . pnf . askolemize
-- 

-- here's where the real work is done.
-- recursively descend, but keep track of the functions seen so far, so
-- as to avoid them when we make up new functions
--
-- the function skolem2 is a partner function, 
-- to allow for the binary connectives
skolem :: Formula Fol -> Vars -> (Formula Fol, [Sym])
skolem fm fns = case fm of
   Exists y p ->
       let xs = U.fv(fm) 
           f = U.variant (if xs == [] then "c_" ++ y else "f_" ++ y) fns 
           fx = Fn f (map Var xs) in
       skolem (U.apply (Map.singleton y fx) p) (f:fns)
   Forall x p -> let (p', fns') = skolem p fns in (Forall x p', fns')
   And p q -> skolem2 And (p, q) fns
   Or p q -> skolem2 Or (p, q) fns
   _ -> (fm, fns)
-- 
skolem2 :: (Formula Fol -> Formula Fol -> Formula Fol) -> (Formula Fol, Formula Fol) 
       -> Vars -> (Formula Fol, [Sym])
skolem2 cons (p,q) fns =
 let (p', fns') = skolem p fns 
     (q', fns'') = skolem q fns' in
 (cons p' q', fns'')



-- just strip off initial universal quats
specialize :: Formula Fol -> Formula Fol 
specialize (Forall _ p) = specialize p
specialize fm = fm
-- 
