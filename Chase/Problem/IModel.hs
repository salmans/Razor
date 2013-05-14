{-| Time-stamp: <2013-04-01 14:28:42 Salman Saghafi>
  This module defines a Model structure that will be used inside a Problem structure. This module can be redefined based on the underlying implementation for models.
-}
module Chase.Problem.IModel where

-- General Modules
import Data.List
import qualified Data.Map as Map

-- Logic Modules
import Formula.SyntaxGeo
import Utils.GeoUtilities

-- Chase Modules:
import Chase.Problem.Observation
import qualified CC.CC as CC

{- Errors raised by this modeule -}
err_ChaseProblemModel_IsTruthDen =
    "Chase.Problem.Model.isTruth: No truth value for Den"
err_ChaseProblemModel_NoEqToDen =
    "Chase.Problem.Model.obsToEquation: Cannot convert Den to Equation"
err_ChaseProblemModel_DelDenotes =
    "Chase.Problem.Model.denotes: Not applicable on Den"
err_ChaseProblemModel_EqlDenotes =
    "Chase.Problem.Model.denotes: Not applicable on Eql"

{-| A modle is a rewrite system together with a set of special constants in the rewrite system.
-}
data Model = Model {
      modelTRS :: CC.TRS,
      modelDomain :: [Term]
}

instance Show Model where
    show (Model trs domain) = 
        "\n" ++
        "Model -----------------------------------\n" ++
        "Domain: " ++ (show (nub dom)) ++ "\n" ++
        "Functions: " ++ (show func) ++ "\n" ++
        "Relations: " ++ (show rel) ++ "\n" ++
        "-----------------------------------------\n"
        where (dom, func, rel) = showFacts trs

{- Shows the values of functions and relations of a model in a pair of
   strings.
-}
showFacts :: CC.TRS -> ([String], [String], [String])
showFacts [] = ([], [], [])
showFacts (r:rs) = 
    case r of
      CC.RW t1@(Fn _ _) t2@(Elm "True") -> 
          ((show t2):restDom, restFunc, (show t1):restRel)
           -- Relation values for terms
      CC.RW t1@(Fn _ _) t2@(Elm _) -> 
          ((show t2):restDom, 
           ((show t1) ++ " = " ++ (show t2)):restFunc, 
           restRel)
          -- Function values for terms
      CC.RW t1@(Elm _) t2@(Elm "True") -> 
          ((show t2):restDom, restFunc, restRel)
          -- Relation values for observations
      CC.RW t1@(Elm _) t2@(Elm _) -> 
          ((show t2):restDom, 
           ((show t1) ++ " = " ++ (show t2)):restFunc, 
           restRel)
          -- Function values for observations
      otherwise -> (restDom, restFunc, restRel)
    where (restDom, restFunc, restRel) = showFacts rs

{-| truth is an special element represented by "True" in models. -}
truth :: Term
truth = Elm "True"

{-| A shorthand for an empty model. -}
empty :: Model
empty = Model [] []

{-| Adds a list of new observations to the given model. It also returns a set of rewrite rules that are introduced to the underlying rewrite system as the new observations are being added.
-}
add :: Model -> [Obs] -> (Model, [CC.RWRule])
add model@(Model trs consts) obs = 
    (Model newTRS newConsts, newRules)
    where ((newTRS, newConsts), newRules) = CC.buildTRS eqs (trs, consts)
          eqs = map obsToEquation obs

{- Convert an obs to a CC.Equation -}
obsToEquation :: Obs -> CC.Equation
obsToEquation (Den t) = error err_ChaseProblemModel_NoEqToDen
obsToEquation (Eql t1 t2) = CC.Eql t1 t2
obsToEquation (Fct a) = CC.Eql (atomToTerm a) truth

{-| Returns true if a term is true in the model; That is, if the Obs is "Fct a", it verifies whether t is true in the model. If the Obs is "Eql t1 t2", it verifies whether t1 and t2 are equal in the model. -} 
isTrue :: Model -> Obs -> Bool
isTrue (Model trs _) obs@(Den t) = 
    case t == truth of -- Since we treat truth as a denotation, truth is an 
                       -- exceptional case.
      True -> True
      False -> error $ err_ChaseProblemModel_IsTruthDen
isTrue (Model trs _) obs@(Fct a) = CC.normalForm trs (obsToTerm obs) == truth
isTrue (Model trs _) (Eql t1 t2) = nf t1 == nf t2
    where nf = CC.normalForm trs

{-| Returns an element in the domain of the model to which a given Fct observation denotes. If the observation is an equation, the function reduces the two sides of the equation to what they denote.
-}
denotes :: Model -> Obs -> Obs
denotes (Model trs _)  obs@(Den t) = 
    if t == truth 
    then obs -- Because truth is an exceptional denotation that can behave 
             -- like a fact, this function is only applicable on truth but 
             -- not other denotations.
    else error err_ChaseProblemModel_DelDenotes
denotes (Model trs _) (Eql t1 t2) = Eql (nf t1) (nf t2)
    where nf = CC.normalForm trs
denotes (Model trs _) obs = 
    if nf == truth
    then termToObs False truth --Always convert truth to Den
    else termToObs True nf
         -- HINT: look at the documentation for termToObs to understand
         -- the first boolean argument.
    where nf = CC.normalForm trs $ obsToTerm obs
