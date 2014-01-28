{-|
  This module defines a Model structure that will be used inside a Problem 
  structure. This module can be redefined based on the underlying 
  implementation for models.
-}
module Chase.Problem.IModel where

-- General Modules
import Data.List
import qualified Data.Map as Map
import Data.Maybe

import Control.Applicative
import qualified Control.Monad.State as State
import Control.DeepSeq

-- Logic Modules
import Formula.SyntaxGeo
import Utils.GeoUtilities

-- Chase Modules:
import Chase.Problem.BaseTypes
import Chase.Problem.Provenance
import Chase.Problem.Observation
import Chase.Problem.RelAlg.RelAlg
import qualified Chase.Problem.RelAlg.Operations as OP
import qualified RelAlg.DB as DB

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
      modelTables   :: Tables,
      modelProvInfo :: ProvInfo
}

instance NFData Model where -- enable strict evaluation for performance analysis
    rnf m = m `seq` ()

instance Show Model where
    show = prettyModel

prettyModel :: Model -> String
prettyModel mdl@(Model tbls _) =
        let list = filter (\(tr, _) -> tr /= DomTable) $ Map.toList tbls
        in
        "Domain: " ++ show (modelDomain mdl) ++ "\n" ++
                      (concat $ (\(tr, t) -> prettyTable tr t) <$> list)


prettyTable :: TableRef -> Table -> String
prettyTable DomTable _             = ""
prettyTable ref@(ConTable sym) tbl = 
    sym ++ " = " ++ (show.head.head) (DB.toList tbl) ++ "\n"
      -- Salman: don't create skolem constants in the first place
prettyTable ref@(FunTable sym) tbl = 
    sym ++ " = " ++ concatMap (prettyRecord ref) (DB.toList tbl) ++ "\n"
prettyTable (RelTable ('@':_)) _   = "" -- internal table (e.g. @Element)
prettyTable ref@(RelTable sym) tbl = 
    sym ++ " = " ++ concatMap (prettyRecord ref) (DB.toList tbl) ++ "\n"

prettyRecord :: TableRef -> Record -> String
prettyRecord DomTable _   = ""
prettyRecord (FunTable sym) rec = 
    prettyTuple (init rec) ++ " -> " ++ show (last rec) ++ " , "
prettyRecord (RelTable sym) rec = prettyTuple rec ++ " , "


prettyTuple :: [Term] -> String
prettyTuple tuple = "(" ++ concat tuple' ++ ")"
    where tuple' = intersperse "," (show <$> tuple)

{-| The domain of a model is a table with key ("*", DomTable)
-}
modelDomain :: Model -> [Term]
modelDomain (Model m _) = 
    concat $ DB.toList $ Map.findWithDefault (DB.Set []) DomTable m

{-| truth is an special element represented by "True" in models. -}
truth :: Term
truth = Elm "True"

{-| A shorthand for an empty model. -}
emptyModel :: Model
emptyModel = Model emptyTables (ProvInfo Map.empty 0)

emptyModelWithElems :: [Term] -> Model
emptyModelWithElems ts = 
    let tbls = Map.fromList ([ (DomTable, es)
                             , (RelTable "@Element", es)] ++ consts)
    in  emptyModel { modelTables = tbls }
    where es = DB.Set $ [[Elm s] | (Fn s _) <- ts']
          consts = [ (ConTable s, DB.Set [[Elm s]]) | (Fn s _) <- ts']
          ts' = filter isConstant ts
          isConstant (Fn _ []) = True
          isConstant (Rn _ []) = True
          isConstant _         = False

{-| Adds a list of new observations to the given model. It also returns a set 
  of tables corresponding to the changes made in the database. It needs a 
  counter value to initialize new constants. Also, it updates the provenance 
  information of the model if it is provided (it is a Just value). That is, the
  function assumes that all of the added observations have the same provenance
  information.
-}
add :: Model -> Int -> [Obs] -> Prov -> (Model, Tables, Int)
add model@(Model tbls provs) c obs prov =
    -- run a counter monad inside a state monad transformer for provenance
    let (((newTables, deltas), (_, newProvs)), c') = State.runState st c
        st = State.runStateT (OP.buildTables eqs tbls emptyTables) 
             (prov, provs)
        newProvs' = case prov of
                      ChaseProv tag _ _ -> newProvs {provInfoLastTag = tag + 1}
                      UserProv          -> newProvs
    in (Model newTables newProvs', deltas, c')
       -- Since we just used the provenance tag, update the tag information.
       -- Salman: make this procedure transparent using a monad.
    where eqs       = map obsToEquation obs
          isFact o  = case o of 
                       Fct _ -> True 
                       otherwise -> False
{- Convert an obs to a Equation -}
obsToEquation :: Obs -> Equation
obsToEquation (Den t)     = error err_ChaseProblemModel_NoEqToDen
obsToEquation (Eql t1 t2) = Equ t1 t2
obsToEquation (Fct a)     = Equ (fromJust (toTerm a)) truth

{-| Returns true if a term is true in the model; That is, if the Obs is 
  "Fct a", it verifies whether t is true in the model. If the Obs is 
  "Eql t1 t2", it verifies whether t1 and t2 are equal in the model. -} 
isTrue :: Model -> Obs -> Bool
isTrue (Model tbls _) obs@(Den t) = 
    case t == truth of -- Since we treat truth as a denotation, truth is an 
                       -- exceptional case.
      True -> True
      False -> error $ err_ChaseProblemModel_IsTruthDen
isTrue (Model tbls _) obs@(Fct (F s ts)) = 
    ts' `elem` (DB.toList $ Map.findWithDefault (DB.Set []) (FunTable s) tbls)
    where ts' = (\t -> case OP.lookupConstant t tbls of 
                         Nothing -> t
                         Just t' -> t') <$> ts
isTrue (Model tbls _) obs@(Fct (R s ts)) = 
    ts' `elem` (DB.toList $ Map.findWithDefault (DB.Set []) (RelTable s) tbls)
    where ts' = (\t -> case OP.lookupConstant t tbls of 
                         Nothing -> t
                         Just t' -> t') <$> ts
isTrue (Model tbls _) (Eql t1 t2) = 
  let evaluate = do
        c1 <- OP.lookupConstant t1 tbls
        c2 <- OP.lookupConstant t2 tbls
        return $ c1 == c2
  in if evaluate == Nothing then False else fromJust evaluate