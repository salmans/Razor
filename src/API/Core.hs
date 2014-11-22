{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-|
  Razor
  Module      : API.Core
  Description : The module provides the core API layer for interacting with the functional core of razor
  Maintainer  : Salman Saghafi, Ryan Danas
-}
module API.Core where
import Chase.Impl
import qualified Chase.Chase
import Chase.Data
import Common.Data
import Common.Basic
import Common.IProvenance
import Common.IObservation
import Common.Model (Model (..))
import Control.Applicative
import Control.Monad
import Data.Maybe
import qualified Data.Map as Map
import Data.List
import qualified Data.Text as T
import SAT.Impl
import Syntax.GeometricUtils
import Syntax.Term
import System.Console.GetOpt
import System.Environment
import System.Exit (exitWith, ExitCode (..))
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)
import Tools.Config
import Tools.Utils (isRealLine)
import Control.Monad.Trans

-- In: command line args
-- Out: configuration options structure
options :: [ OptDescr (Config -> IO Config) ]
options =
    [ Option "i" ["input", "file"]
        (ReqArg
            (\arg cfg -> return cfg { configInput = Just arg })
            "FILE")
        "In theory file"
    , Option "d" ["debug"]
        (NoArg
            (\cfg -> return cfg { configDebug = True }))
        "Debug mode"
    , Option "s" ["skolem-depth"]
        (OptArg
            (\arg cfg -> return $ 
                         case join (readMaybe <$> arg) of
                           Nothing -> cfg
                           Just sk -> cfg { configSkolemDepth = sk})
            "#")
        "Depth of skolem term for reusing elements (-1 for pure minimal models)"
    , Option "c" ["command", "cmd"]
        (ReqArg
            (\arg cfg -> return cfg { configCommand = Just arg })
            "String")
        "UserSyntax CLI Command"
    , Option "x" ["state"]
        (ReqArg
            (\arg cfg -> return cfg { configState = Just arg })
            "FILEPATH")
        "UserState represented as an XML file (program writes to this file)"
    , Option "h" ["help"]
        (NoArg
          (\_ -> do
            prg <- getProgName
            hPutStrLn stderr (usageInfo prg options)
            exitWith ExitSuccess))
        "Show help"
    ]
parseConfig :: [String] -> IO Config
parseConfig args = do
  -- Parse options, getting a list of option actions
  let (actions, nonOptions, errors) = getOpt RequireOrder options args
  -- Here we thread startOptions through all supplied option actions
  config <- foldl (>>=) (return defaultConfig) actions
  return config
-- In: configuration options, user input theory
-- Out: a theory if parsing success
parseTheory :: Config -> String -> IO (Maybe Theory)
parseTheory config input = do
	-- let inputLines = lines input
        let sequents = parseGeometricTheory input -- inputLines
        return $ Just sequents
-- In: configuration, theory
-- Out: G*, which consists of ground facts, provenance info, and a propositional theory
generateGS :: Config -> Theory -> (ChaseHerbrandBaseType, ProvInfo, SATTheoryType)
generateGS config theory = chase config theory
-- In: G*, new observation
-- Out: an augmented G* with the new observation
augment :: Config -> Theory -> (ChaseHerbrandBaseType, ProvInfo, SATTheoryType) -> Observation -> (ChaseHerbrandBaseType, ProvInfo, SATTheoryType)
augment cfg thy (b, p, t) obs = do
  let thy' = preprocess thy
  let seqMap = (buildSequentMap $ fromJust <$> fromSequent <$> thy') :: SequentMap ChaseSequentType
  let (b', p', t') = augmentGS cfg seqMap (b, p, t) obs
  let newSeq = ObservationSequent [] [[obs]]
  let t'' = storeSequent t' (UserBlame obs, newSeq)
  (b', p', t'')
--
--
augmentGS :: Config -> SequentMap ChaseSequentType -> (ChaseHerbrandBaseType, ProvInfo, SATTheoryType) -> Observation -> (ChaseHerbrandBaseType, ProvInfo, SATTheoryType)
augmentGS _ _ gs eqobs@(Obs (Rel "=" terms)) = gs
augmentGS cfg seqMap (b, p, t) obs@(Obs (Rel rsym terms)) = do
  let d = addToBase obs emptyBase
  Chase.Chase.resumeChase cfg seqMap b d p t
-- In: a propositional theory
-- Out: an iterator that can be used to sequentially generate models (model stream)
modelStream :: SATTheoryType -> SATIteratorType
modelStream propThy = satInitialize propThy
-- In: a model stream
-- Out: an updated model stream and the next model
nextModel :: SATIteratorType -> (Maybe Model, SATIteratorType)
nextModel it = (satSolve it)

----------------
-- PROVENANCE --
----------------
--
--
getBlamedSequent :: SATTheoryType -> Blame -> Maybe Sequent
getBlamedSequent satthy blame = case blameSequent satthy blame of
  Nothing -> Nothing
  Just oseq -> Just $ toSequent oseq
--
--
getElementBlames :: Theory -> ElementProvs -> Model -> [Element] -> [(Blame, [Element])]
getElementBlames thy prov mdl eqelms = do
  let elm = head eqelms
  case catMaybes $ map (getSkolemTree prov mdl) eqelms of
    [] -> []
    trees -> do
      tree@(e, f, r) <- trees
      let irules = map (\r->((fromMaybe 0 (elemIndex r (preprocess thy)))+1, r)) (preprocess thy)
      let blames = catMaybes $ map (\rule->getElementBlame rule mdl tree) irules
      return ((head blames), r)
--
--
getElementBlame :: (Id, Sequent) -> Model -> (Element, FnSym, [Element]) -> Maybe Blame
getElementBlame (rid, rule@(Sequent bd hd)) mdl (e, f, r) = do
  let exists = headExistentials hd 0
  let ruleskolems = map (\(name, _, _)->name) exists
  case elem f ruleskolems of
    False -> Nothing
    True -> do
      let frees = freeVars rule
      let terms = map (\e->(Elem e)) r
      Just $ TheoryBlame rid (Map.fromList $ zip frees terms)
--
--
getObservationBlame :: ObservationProvs -> Model -> Observation -> Maybe Blame
getObservationBlame prov mdl obv@(Obs (Rel sym ts)) = do
  let eqelms = (map (getEqualElements mdl) ts)
  let possibilities = combination eqelms
  let obvs = map (\ts->(Obs (Rel sym (map(\t->(Elem t))ts)))) possibilities
  let blames = catMaybes $ map (\o->(Map.lookup o prov)) obvs
  case blames of
    [] -> Nothing
    blame:bs -> Just blame
getObservationBlame _ _ _ = Nothing
--
--
combination :: [[a]] -> [[a]]
combination [l] = do
  choose <- l
  return (return choose)
combination (l:ls) = do
  choose <- l
  rest <- (combination ls)
  return (choose : rest)
combination [] = []

-------------------
-- OTHER HELPERS --
-------------------
-- In: model of a theory, a term representing an element
-- Out: a list of elements this model is equivalent to in the given model
getEqualElements :: Model -> Term -> [Element]
getEqualElements mdl term = case term of
  (Elem e) -> case (Map.lookup e (modelElements mdl)) of
    Nothing -> []
    Just eqelms -> eqelms
  _ -> []
--
--
getSkolemHead :: Term -> Maybe (FnSym, [Term])
getSkolemHead skolemtree = do
  case skolemtree of
    (Fn skolemhead skolemrest) -> Just (skolemhead, skolemrest)
    (Cons (Constant skolemhead)) -> Just (skolemhead, [])
    _ -> Nothing
--
--
getSkolemElement :: ElementProvs -> Term -> Maybe Element
getSkolemElement prov skolemterm = (findElementWithProv skolemterm prov)
--
--
getSkolemTree :: ElementProvs -> Model -> Element -> Maybe (Element, FnSym, [Element])
getSkolemTree prov mdl elm = case (getElementProv elm prov) of
  [] -> Nothing
  trees -> case (getSkolemHead (head trees)) of
    Nothing -> Nothing
    Just (skolemhead, skolemrest) -> Just (elm, skolemhead, (concatMap (\t->(maybeToList (getSkolemElement prov t))) skolemrest))
--
-- 
headExistentials :: Formula -> Int -> [(FnSym, Int, Variable)]
headExistentials Tru _                   = []
headExistentials Fls _                   = []
headExistentials (And f1 f2) i           = 
    headExistentials f1 i ++ headExistentials f2 i
headExistentials (Or f1 f2) i            = 
    headExistentials f1 i ++ headExistentials f2 (i+1)
headExistentials (Atm a) _                = []
headExistentials (Exists (Just fn) v f) i = (fn, i, v):(headExistentials f i)
headExistentials (Exists Nothing _ f) i  = headExistentials f i
headExistentials (Lone (Just fn) v f unq) i = (fn, i, v):(headExistentials f i)
headExistentials (Lone Nothing _ f _) i  = headExistentials f i
--
-- an observation (for now) is just an atom consisting of only elements
getObservation :: Formula -> Maybe Observation
getObservation (Atm atm@(Rel rsym terms)) = do
  let elms = concat (map (\t->maybeToList (termToElement t)) terms)
  if (length terms) == (length elms)
    then case toObservation atm of
      Just obv@(Obs (Rel rsym terms)) -> Just obv
      _ -> Nothing
    else Nothing
getObservation _ = Nothing
