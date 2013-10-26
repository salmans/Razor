{-|
  This module defines a Model structure that will be used inside a Problem 
  structure. This module can be redefined based on the underlying implementation 
  for models.
-}
module Chase.Problem.IModel where

-- General Modules
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Control.Applicative

-- Logic Modules
import Formula.SyntaxGeo
import Utils.GeoUtilities

-- Chase Modules:
import Chase.Problem.Observation
import Chase.Problem.RelAlg.RelAlg
import qualified Chase.Problem.RelAlg.Operations as OP
import qualified RelAlg.DB as DB

-- Other Modules
import Debug.Trace

{- Errors raised by this modeule -}
err_ChaseProblemModel_IsTruthDen =
    "Chase.Problem.Model.isTruth: No truth value for Den"
err_ChaseProblemModel_NoEqToDen =
    "Chase.Problem.Model.obsToEquation: Cannot convert Den to Equation"
err_ChaseProblemModel_DelDenotes =
    "Chase.Problem.Model.denotes: Not applicable on Den"
err_ChaseProblemModel_EqlDenotes =
    "Chase.Problem.Model.denotes: Not applicable on Eql"


{-| A modle is a set of tables.
-}
data Model = Model{
      modelTables    :: Tables
}

instance Show Model where
    show mdl@(Model tbls) = 
        let list = filter (\(tr, _) -> tr /= DomTable) $ Map.toList tbls
        in
        "Domain: " ++ show (modelDomain mdl) ++ "\n" ++
                      (concat $ (\(tr, t) -> showTable tr t) <$> list)

showTable  DomTable tbl      = []
showTable (RelTable sym) tbl = show sym ++ " = "
                               ++ show (DB.toList tbl) ++ "\n"
showTable (FunTable sym) tbl = show sym ++ " = "
                               ++ show (DB.toList tbl) ++ "\n"

{-| The domain of a model is a table with key ("*", DomTable)
-}
modelDomain :: Model -> [Term]
modelDomain (Model m) = 
    concat $ DB.toList $ Map.findWithDefault (DB.Set []) DomTable m

{-| truth is an special element represented by "True" in models. -}
truth :: Term
truth = Elm "True"

{-| A shorthand for an empty model. -}
empty :: Model
empty = Model emptyTables
--empty = Model [] [truth]

{-| Adds a list of new observations to the given model. It also returns a set 
  of tables corresponding to the changes made in the database.
-}
add :: Model -> [Obs] -> (Model, Tables)
add model@(Model tbls) obs =     
    (Model newTables, changes)
    where (newTables, changes) = OP.buildTables eqs tbls
          eqs                 = map obsToEquation obs

{- Convert an obs to a Equation -}
obsToEquation :: Obs -> Equation
obsToEquation (Den t)     = error err_ChaseProblemModel_NoEqToDen
obsToEquation (Eql t1 t2) = Equ t1 t2
obsToEquation (Fct a)     = Equ (fromJust (toTerm a)) truth

{-| Returns true if a term is true in the model; That is, if the Obs is 
  "Fct a", it verifies whether t is true in the model. If the Obs is 
  "Eql t1 t2", it verifies whether t1 and t2 are equal in the model. -} 
isTrue :: Model -> Obs -> Bool
isTrue (Model tbls) obs@(Den t) = 
    case t == truth of -- Since we treat truth as a denotation, truth is an 
                       -- exceptional case.
      True -> True
      False -> error $ err_ChaseProblemModel_IsTruthDen
isTrue (Model tbls) obs@(Fct (R s ts)) = 
    ts' `elem` (DB.toList $ Map.findWithDefault (DB.Set []) (RelTable s) tbls)
    where ts' = (\t -> case OP.lookupConstant t tbls of 
                         Nothing -> t
                         Just t' -> t') <$> ts
isTrue (Model tbls) (Eql t1 t2) = 
    if evaluate == Nothing then False else True
    where constOf t = OP.lookupConstant t tbls
          evaluate = do
            c1 <- constOf t1
            c2 <- constOf t2
            return $ c1 == c2