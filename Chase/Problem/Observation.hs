{-| Time-stamp: <2013-02-21 18:18:20 Salman Saghafi>
  An observation defines a data structure that denotes an element in the model as well as the equality between to elements.
-}
module Chase.Problem.Observation where

import Data.List

import Formula.SyntaxGeo
import Utils.GeoUtilities
import Tools.GeoUnification(matchSubterms, match) 
import Debug.Trace
import Data.Char

{-| Errors raised by this modeule |-}
err_ChaseProblemObservation_VarToAtm =
    "Chase.Problem.Observation.termToAtom: A variable cannot be converted " ++
    "to an atom"
err_ChaseProblemObservation_RepWithFct = 
    "Chase.Problem.Observation.replaceObsSubterms: Fact observations cannot " ++
    "appear inside other observations."

{-| An observation falls into the following three categories:
  1. A term denotes an element in a model 
  2. Two terms are equal in a model
  3. A relational fact is a in the model
-}
data Obs = Den Term -- Term denotes an element
         | Eql Term Term -- Two terms are equal
         | Fct Atom -- Atm is true as a fact
           deriving (Eq, Ord)

instance Show Obs where
    show (Den t) = "|" ++ (show t) ++ "|"
    show (Eql t1 t2) = "|" ++ (show t1) ++ "| = |" ++ (show t2) ++ "|"
    show (Fct a) = "{" ++ (show a) ++ "}"

instance Fv Obs where
    fv (Den t) = fv t
    fv (Eql t1 t2) = union (fv t1) (fv t2)
    fv (Fct a) = fv a

{-| Applies a function (Term -> Term) to an Obs.
-}
onObs :: (Term -> Term) -> Obs -> Obs
onObs f (Den t) = Den (f t)
onObs f (Eql t1 t2) = Eql (f t1) (f t2)
onObs f (Fct a) = Fct $ onAtom f a

{-| Returns a list of symbols used in an Obs. 
-}
obsFuncSyms :: Obs -> [Sym]
obsFuncSyms obs = termFuncSyms $ obsToTerm obs

{-| Returns a list of symbols inside an Obs. 
-}
obsSym :: Obs -> Sym
obsSym obs = case obsToTerm obs of
               Fn sym _ -> sym
               Elm sym -> sym

{-| Retunrs a list of subterms of an input obs wrapped in obs.
-}
{- This is one of those odd situations resulted from different ways of
   treating observations in the model. The model stor observations as
   terms but observations do not have to be terms!
   One of the consequences is that when matching, we still need to convert
   observations to terms and perform matching. That can cause situations like
   below where subterms of a denoting observations are computed more simply
   than equality and fact observations.
-}
obsSubterms :: Obs -> [Obs]
obsSubterms obs@(Fct _) = 
    obs:map (termToObs False) ts
    where ts = filter (\t -> t /= toTerm) (subterms toTerm)
          toTerm = obsToTerm obs
obsSubterms obs@(Eql _ _) = 
    obs:map (termToObs False) ts
    where ts = filter (\t -> t /= toTerm) (subterms toTerm)
          toTerm = obsToTerm obs
obsSubterms obs@(Den _) = map (termToObs False) $ subterms $ obsToTerm obs

{-| Just like Utils.GeoUtilities.replaceSubterms replace subterms but it happens within an Obs.
-}
replaceObsSubterms :: Obs -> Obs -> Obs -> Obs
replaceObsSubterms (Fct _) _ _ = error err_ChaseProblemObservation_RepWithFct
-- If the destination observation is a fact, the result is a fact observation
replaceObsSubterms obs1 obs2 obs3@(Fct _) =
    termToObs True $ replaceSubterms 
                  (obsToTerm obs1) (obsToTerm obs2) (obsToTerm obs3)
-- If not, the result is an equation if obs3 is an equation (that happens 
-- naturally) and the result is a denotation otherwise.
replaceObsSubterms obs1 obs2 obs3 = 
    termToObs False $ replaceSubterms 
                  (obsToTerm obs1) (obsToTerm obs2) (obsToTerm obs3)

{-| Returns a substitution that matches the second observation to the first observation if such substitution exists. Otherwise, it returns Nothing.
-}
matchObs :: Obs -> Obs -> Maybe Sub
matchObs obs1 obs2 = match [(t1, t2)]
    where t1 = obsToTerm obs1
          t2 = obsToTerm obs2

{-| Returns a list of substitutions that match a subterm in the second obs to the first obs.
-}
matchObsSubterms :: Obs -> Obs -> [Sub]
matchObsSubterms obs1 obs2 = 
    matchSubterms (obsToTerm obs1) (obsToTerm obs2)

{-| Converts an Obs to a Term. This function is primarily used to make it possible to use Term matching functions over Obs.
-}
obsToTerm :: Obs -> Term
obsToTerm (Den t) = t
obsToTerm (Eql t1 t2) = Fn "=" [t1, t2]
obsToTerm (Fct a) = atomToTerm a

{-| Converts a Term to a Obs. The purpose of this function is to convert terms that have been produced using obsToTerm back to their Obs form.
  NOTE: determining whether a term represents a fact or a denotaion is not possible. For now, the caller passes an extra parameter to determine whether the output has to be a fact or a denotation.
-}
termToObs :: Bool -> Term -> Obs
termToObs _ (Fn "=" [t1, t2]) = Eql t1 t2
termToObs _ (Fn "=" _) = error $ "Chase.Problem.Operations.termToObs: " ++
                         "equality must have only to parameters!"
termToObs isFact t = 
      case isFact of 
        True -> Fct $ termToAtom t
        False -> Den t


{-| Creates a term from an atom. |-}
atomToTerm :: Atom -> Term
atomToTerm (R sym terms) = Fn sym terms

{-| Creates an atom from a term |-}
termToAtom :: Term -> Atom
termToAtom (Var _) = error $ err_ChaseProblemObservation_VarToAtm
termToAtom (Fn sym ts) = R sym ts
termToAtom (Elm elm) = R elm []