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
import Chase.Problem.RelAlg.RelAlg
import qualified Chase.Problem.RelAlg.Operations as OP
import qualified RelAlg.DB as DB

{- Errors raised by this modeule -}
err_ChaseProblemModel_IsTruthDen =
    "Chase.Problem.Model.isTruth: No truth value for Den"
err_ChaseProblemModel_EqlDenotes =
    "Chase.Problem.Model.denotes: Not applicable on Eql"

{-| A modle is a set of tables.
-}
data Model = Model{
      modelTables   :: Tables,
      modelProvInfo :: ProvInfo,
      modelElemHist :: ElemHistory
}

instance NFData Model where -- enable strict evaluation for performance analysis
    rnf m = m `seq` ()

instance Show Model where
    show = prettyModel

prettyModel :: Model -> String
prettyModel mdl@(Model tbls _ _) =
        let list = filter (\(tr, _) -> tr /= DomTable) $ Map.toList tbls
        in
        "Domain: " ++ show (modelDomain mdl) ++ "\n" ++
                      (concat $ (\(tr, t) -> prettyTable tr t) <$> list)

-- FOR TESTING PURPOSES:
--                      ++ "\n" ++ greaterInfo mdl


greaterInfo :: Model -> String
greaterInfo mdl = 
    let lines = (\(x, y) -> 
                     if   greaterElem mdl x y
                     then (show x) ++ " >  " ++ (show y)
                     else (show x) ++ " ~> " ++ (show y)) <$> pairs
    in  concatMap (\l -> (show l)  ++ "\n") lines
    where dom   = modelDomain mdl
          pairs = [(x, y) | x <- dom, y <- dom]

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


prettyTuple :: [Elem] -> String
prettyTuple tuple = "(" ++ concat tuple' ++ ")"
    where tuple' = intersperse "," (show <$> tuple)

{-| The domain of a model is a table with key ("*", DomTable)
-}
modelDomain :: Model -> [Elem]
modelDomain (Model m _ _) = 
    concat $ DB.toList $ Map.findWithDefault (DB.Set []) DomTable m

{-| Returns the original tables of the model, filtering out the tables 
  constructed during preprocessing. -}
modelOrigTables :: Model -> Tables
modelOrigTables mdl = Map.filterWithKey filterFunc $ modelTables mdl
    where filterFunc (ConTable ('@':_)) _ = False
          filterFunc (RelTable ('@':_)) _ = False
          filterFunc (FunTable ('@':_)) _ = False
          filterFunc _ _                  = True  -- otherwise

{-| truth is an special element represented by "True" in models. -}
truth :: Term
truth = Elm $ Elem "True"

{-| A shorthand for an empty model. -}
emptyModel :: Model
emptyModel = Model emptyTables (ProvInfo Map.empty 0) Map.empty

emptyModelWithElems :: [Elem] -> Model
emptyModelWithElems ts = 
    let tbls = Map.fromList ([ (DomTable, es)
                             , (RelTable "@Element", es)] ++ consts)
    in  emptyModel { modelTables = tbls }
    where es = DB.Set $ [ts]
          consts = [ (ConTable s, DB.Set [[t]]) | t@(Elem s) <- ts]

{-| Adds a list of new observations to the given model. It also returns a set 
  of tables corresponding to the changes made in the database. It needs a 
  counter value to initialize new constants. Also, it updates the provenance 
  information of the model if it is provided (it is a Just value). That is, the
  function assumes that all of the added observations have the same provenance
  information.
-}
add :: Model -> Int -> [Obs] -> Prov -> (Model, Tables, Int)
add model@(Model tbls provs hist) c obs prov =
    -- run a counter monad inside a state monad transformer for provenance
    let (((newTables, deltas), (_, newProvs)), c') = State.runState st c
        st = State.runStateT (OP.buildTables obs tbls emptyTables) 
             (prov, provs)
        newProvs' = case prov of
                      ChaseProv tag _ _ -> newProvs {provInfoLastTag = tag + 1}
                      UserProv          -> newProvs
    in (Model newTables newProvs' hist, deltas, c')
       -- Since we just used the provenance tag, update the tag information.
       -- Salman: make this procedure transparent using a monad.

{-| Returns true if a term is true in the model; That is, if the Obs is 
  "Fct a", it verifies whether t is true in the model. If the Obs is 
  "Eql t1 t2", it verifies whether t1 and t2 are equal in the model. -} 
isTrue :: Model -> Obs -> Bool
isTrue (Model tbls _ _) obs@(Fct (R s ts)) = 
    let ts' = mapM ((flip OP.lookupConstant) tbls) ts
        tbl = (DB.toList $ Map.findWithDefault (DB.Set []) (RelTable s) tbls)
    in  if   ts' == Nothing
        then False
        else (fromJust ts') `elem` tbl
isTrue (Model tbls _ _) obs@(Fct (F s ts)) = 
    let ts' = mapM ((flip OP.lookupConstant) tbls) ts
        tbl = (DB.toList $ Map.findWithDefault (DB.Set []) (FunTable s) tbls)
    in  if   ts' == Nothing
        then False
        else (fromJust ts') `elem` tbl
isTrue (Model tbls _ _) (Eql t1 t2) = 
  let evaluate = do
        c1 <- OP.lookupConstant t1 tbls
        c2 <- OP.lookupConstant t2 tbls
        return $ c1 == c2
  in if evaluate == Nothing then False else fromJust evaluate

--------------------------------------------------------------------------------
{- Defines a preorder on the elements of the model regarding the amount of 
   information they carry in the model. -}
greaterElem :: Model -> Elem -> Elem -> Bool
greaterElem = (flip greaterElemHelper) [] 
  -- Run the helper function with empty assumptions.

{- A helper for graterElem, which also accepts a set of assumptions about the 
   ordering relation on the elements of the model. -}
greaterElemHelper :: Model -> [(Elem, Elem)] -> Elem -> Elem -> Bool
greaterElemHelper mdl assumpts e1 e2 = 
    all (\(_, tbl) -> greaterElemInTable mdl tbl ((e1, e2):assumpts) e1 e2) list
    -- For all tables in the model, e1 has to be greater than e2 assuming that
    -- e1 is greater than e2. That is, the assumption that e1 is greater than e2
    -- is consistent in the model.
    where tbls = modelTables mdl
          list = Map.toList tbls

{- Computes the information ordering relation on two elements with respect to 
   only one table in the model. -}
greaterElemInTable :: Model -> Table -> [(Elem, Elem)] -> Elem -> Elem -> Bool
greaterElemInTable mdl tbl assumpts e1 e2 = 
    let rows   = DB.toList tbl
        e1rows = filter (e1 `elem`) rows -- Get all rows containing e1
        e2rows = filter (e2 `elem`) rows -- Get all rows containing e2
        disj   = null $ filter (\r -> (e1 `elem` r) && (e2 `elem` r)) rows
    in  e1 /= e2 && all (\t2 -> any (\t1 -> greaterTuple t1 t2) e1rows) e2rows
        -- For all tuples in e2rows, there is a greater tuple in e1rows.
    where greaterTuple t t' = and $ zipWith greater t t'
                              -- Tuple t is greater than tuple t' if all 
                              -- elements in t are greater than the elements in 
                              -- their similar position in t'.
          greater e e'      = (e, e') `elem` assumpts || 
                              greaterElemHelper mdl assumpts e e'
                              -- Element e is greater than element e' if either
                              -- it is assumed by the assumptions, or e is 
                              -- greater than e' given the current set of 
                              -- assumptions.