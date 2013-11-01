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

{-| An observation falls into the following three categories:
  1. A term denotes an element in a model 
  2. Two terms are equal in a model
  3. A relational fact is in the model
-}
data Obs = Den Term -- Term denotes an element
         | Eql Term Term -- Two terms are equal
         | Fct Atom -- Atm is true as a fact
           deriving (Eq, Ord)

instance Show Obs where
    show (Den t) = "|" ++ (show t) ++ "|"
    show (Eql t1 t2) = "|" ++ (show t1) ++ "| = |" ++ (show t2) ++ "|"
    show (Fct a) = "|" ++ (show a) ++ "|"

instance TermBased Obs where
    liftTerm f (Den t) = Den (f t)
    liftTerm f (Eql t1 t2) = Eql (f t1) (f t2)
    liftTerm f (Fct a) = Fct $ liftTerm f a

    freeVars (Den t) = freeVars t
    freeVars (Eql t1 t2) = union (freeVars t1) (freeVars t2)
    freeVars (Fct a) = freeVars a

{-| Returns a list of symbols used in an Obs. 
-}
obsFuncSyms :: Obs -> [Sym]
obsFuncSyms obs = termFuncSyms $ obsToTerm obs
                  
{-| Converts an Obs to a Term. This function is primarily used to make it 
  possible to use Term matching functions over Obs.
-}
obsToTerm :: Obs -> Term
obsToTerm (Den t) = t
obsToTerm (Eql t1 t2) = Fn "=" [t1, t2]
obsToTerm (Fct a) = fromJust $ toTerm a

{-| Converts a Term to a Obs. The purpose of this function is to convert terms 
  that have been produced using obsToTerm back to their Obs form.
  NOTE: determining whether a term represents a fact or a denotaion is not 
  possible. For now, the caller passes an extra parameter to determine whether 
  the output has to be a fact or a denotation.
-}
termToObs :: Bool -> Term -> Obs
termToObs _ (Rn "=" [t1, t2]) = Eql t1 t2
termToObs _ (Rn "=" _) = error $ "Chase.Problem.Operations.termToObs: " ++
                         "equality must have only two parameters!"
termToObs isFact t = 
      case isFact of 
        True -> Fct $ fromJust $ fromTerm t
        False -> Den t