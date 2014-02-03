{-|
  An observation defines a data structure that denotes an element in the model 
  as well as the equality between to elements.
-}
module Chase.Problem.Observation where

import Data.List

import Formula.SyntaxGeo
import Utils.GeoUtilities
import Tools.GeoUnification(matchSubterms, match) 

import Data.Maybe
import Data.Char

{-| Errors raised by this modeule |-}
err_ChaseProblemObservation_VarToAtm =
    "Chase.Problem.Observation.termToAtom: A variable cannot be converted " ++
    "to an atom"
err_ChaseProblemObservation_RepWithFct = 
    "Chase.Problem.Observation.replaceObsSubterms: Fact observations cannot " ++
    "appear inside other observations."
err_ChaseProblemObservations_atomToObs_EqTwoParam =
    "Chase.Problem.Operations.formulaHolds: " ++
    "equality must have only two parameters!"

{-| An observation falls into the following three categories:
  1. A term denotes an element in a model 
  2. Two terms are equal in a model
  3. A relational fact is in the model
-}
data Obs = Eql Term Term -- Two terms are equal
         | Fct Atom -- Atm is true as a fact
           deriving (Eq, Ord)

instance Show Obs where
    show (Eql t1 t2) = "|" ++ (show t1) ++ "| = |" ++ (show t2) ++ "|"
    show (Fct a) = "|" ++ (show a) ++ "|"

instance TermBased Obs where
    liftTerm f (Eql t1 t2) = Eql (f t1) (f t2)
    liftTerm f (Fct a) = Fct $ liftTerm f a

    freeVars (Eql t1 t2) = union (freeVars t1) (freeVars t2)
    freeVars (Fct a) = freeVars a

{-| Returns a list of symbols used in an Obs. 
-}
obsFuncSyms :: Obs -> [Sym]
obsFuncSyms (Eql t1 t2)    = termFuncSyms t1 `union` termFuncSyms t2
obsFuncSyms (Fct (R _ ts)) = nub $ concatMap termFuncSyms ts
obsFuncSyms (Fct (F _ ts)) = nub $ concatMap termFuncSyms ts
                  
{-| Converts an Atom to an Obs. -}
atomToObs :: Atom -> Obs
atomToObs (R "=" [t1, t2]) = Eql t1 t2
atomToObs (R "=" _)        = 
    error err_ChaseProblemObservations_atomToObs_EqTwoParam
atomToObs atm              = Fct atm