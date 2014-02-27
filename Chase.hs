module Chase where

import Control.Applicative ((<$>))
import qualified Data.Map as Map
import qualified Text.Parsec as Parsec

import qualified Datatypes
import qualified Chase.Chase as Chase
import qualified Chase.Formula.SyntaxGeo as SyntaxGeo
import qualified Chase.Problem.Model as Model
import qualified Chase.Problem.Observation as Observation
import qualified Chase.Problem.Operations as Operations
import qualified Chase.Problem.Provenance as Provenance
import qualified Chase.Problem.Structures as Structures
import qualified Chase.Problem.RelAlg.RelAlg as RelAlg
import qualified Chase.RelAlg.DB as DB
import qualified Chase.Tools.Config as Config
import Utility ((!!!))
import qualified WebParse

import Datatypes

chase :: Datatypes.GraphLoc -> Maybe Datatypes.Model
chase (Datatypes.GraphLoc theory index steps) =
  let (frms,initialProblem) =
        Operations.buildProblem $ map chasifySequent theory 
      nextStep row stepIndex stepsLeft = do 
        currentNode <- row !!! stepIndex
        case stepsLeft of
          [] -> return currentNode
          (constraint,nextIndex):restSteps ->
            let (newModel,_,newConst) =
                  (Model.add (Structures.problemModel currentNode)
                   (Structures.problemLastConstant currentNode)
                   [chasifyConstraint constraint] Provenance.UserProv) in
            (nextStep
             (Chase.runChaseWithProblem Config.defaultConfig frms
              currentNode {Structures.problemModel = newModel,
                           Structures.problemLastConstant = newConst})
             nextIndex restSteps) in
  dechasifyModel <$>
  nextStep (Chase.runChase Config.defaultConfig Nothing frms initialProblem) 
  index steps

chasifySequent :: Datatypes.Sequent -> SyntaxGeo.Sequent
chasifySequent (Datatypes.Sequent premises consequents) = SyntaxGeo.Sequent {
  SyntaxGeo.sequentBody = chasifyConj chasifyAtomicFormula premises,
  SyntaxGeo.sequentHead = chasifyDisj chasifyExQuant consequents
  }

chasifyDisj :: (a -> SyntaxGeo.Formula) -> [a] -> SyntaxGeo.Formula
chasifyDisj f = chasifyConnective f SyntaxGeo.Or SyntaxGeo.Fls

chasifyConj :: (a -> SyntaxGeo.Formula) -> [a] -> SyntaxGeo.Formula
chasifyConj f = chasifyConnective f SyntaxGeo.And SyntaxGeo.Tru

chasifyConnective :: (a -> SyntaxGeo.Formula)
                     -> (SyntaxGeo.Formula -> SyntaxGeo.Formula
                         -> SyntaxGeo.Formula)
                     -> SyntaxGeo.Formula -> [a] -> SyntaxGeo.Formula
chasifyConnective _ _ base [] = base
chasifyConnective func connective _ xs = foldr1 connective $ map func xs

chasifyExQuant :: ([Datatypes.VariableSymbol], [Datatypes.TAtom])
                  -> SyntaxGeo.Formula
chasifyExQuant (vars,atom) =
  foldr SyntaxGeo.Exists (chasifyConj chasifyAtomicFormula atom) $ do
    Datatypes.VariableSymbol symname <- vars
    return symname

chasifyAtomicFormula :: Datatypes.TAtom -> SyntaxGeo.Formula
chasifyAtomicFormula = SyntaxGeo.Atm . chasifyTAtom

chasifyTAtom :: Datatypes.TAtom -> SyntaxGeo.Atom
chasifyTAtom atom = case atom of
  Datatypes.TPredicate (Datatypes.PredicateSymbol symname) args ->
    SyntaxGeo.R symname $ map chasifyTTerm args
  Datatypes.TEquality term1 term2 -> 
    SyntaxGeo.R "=" [chasifyTTerm term1,chasifyTTerm term2]

chasifyTTerm :: Datatypes.TTerm -> SyntaxGeo.Term
chasifyTTerm term = case term of
  Datatypes.TVariable (Datatypes.VariableSymbol symname) ->
    SyntaxGeo.Var symname
  Datatypes.TFunction (Datatypes.FunctionSymbol symname) args ->
    SyntaxGeo.Fn symname $ map chasifyTTerm args

chasifyConstraint :: Datatypes.MAtom -> Observation.Obs
chasifyConstraint constraint = case constraint of
  Datatypes.MPredicate (Datatypes.PredicateSymbol symname) args -> 
    Observation.Fct $ SyntaxGeo.R symname $ map chasifyMTerm args
  Datatypes.MEquality term1 term2 ->
    Observation.Eql (chasifyMTerm term1) (chasifyMTerm term2)

chasifyMTerm :: Datatypes.MTerm -> SyntaxGeo.Term
chasifyMTerm term = case term of
  Datatypes.MVariable (Datatypes.VariableSymbol symname) ->
    SyntaxGeo.Var symname
  Datatypes.MFunction (Datatypes.FunctionSymbol symname) args ->
    SyntaxGeo.Fn symname $ map chasifyMTerm args
  Datatypes.ModelElement (Datatypes.ModelElementSymbol symnum) ->
    SyntaxGeo.Elm $ SyntaxGeo.Elem $ "e#" ++ show symnum

dechasifyModel :: Structures.Problem -> Datatypes.Model
dechasifyModel Structures.Problem {Structures.problemModel = mdl} =
  Datatypes.Model (map dechasifyModelElement $ Model.modelDomain mdl) $ do
    (ref,tbl) <- Map.assocs $ Model.modelTables mdl
    let rels = map (map dechasifyModelElement) $ DB.toList tbl
    case ref of
      RelAlg.ConTable symname -> let [[constValue]] = rels in
        return $
        Datatypes.FunctionFact (Datatypes.FunctionSymbol symname) [] constValue
      RelAlg.RelTable ('@':_) -> []
      RelAlg.RelTable symname -> Datatypes.PredicateFact (Datatypes.PredicateSymbol symname) <$> rels
      RelAlg.FunTable symname -> do
        row <- rels
        return $ Datatypes.FunctionFact (Datatypes.FunctionSymbol symname) (init row) (last row)
      RelAlg.DomTable -> []

dechasifyModelElement :: SyntaxGeo.Elem -> Datatypes.ModelElementSymbol
dechasifyModelElement (SyntaxGeo.Elem ('e':'#':symnum)) =
  Datatypes.ModelElementSymbol $ read symnum