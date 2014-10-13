{-|
  Razor
  Module      : API
  Description : The module provides a stateless API for (G)UI applications to use for interacting with the functional core of Razor
  Maintainer  : Salman Saghafi, Ryan Danas
-}
{-| API Cleanup TODO
	make this an interface and this module the implementation?
	options should not call exit?
-}
module API where
import Chase.Impl
import Common.Basic
import Common.Provenance
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
import Tools.FolToGeo (parseFolToSequents)
import Tools.Utils (isRealLine)



-- In: command line args
-- Out: configuration options structure
options :: [ OptDescr (Config -> IO Config) ]
options =
    [ Option "i" ["input", "file"]
        (ReqArg
            (\arg cfg -> return cfg { configInput = Just arg })
            "FILE")
        "In theory file"
    , Option "b" ["bound"]
        (OptArg
            (\arg cfg -> return $
                         case join (readMaybe <$> arg) of
                           Nothing -> cfg
                           Just b  -> cfg { configBound = Just b})
            "#")
        "Maximum for bounded model-finding"
    , Option "d" ["debug"]
        (NoArg
            (\cfg -> return cfg { configDebug = True }))
        "Debug mode"

    , Option "n" ["incremental"]
        (NoArg
            (\cfg -> return cfg { configIncremental = True }))
        "Enable incremental view maintenance"
 
    , Option "1" ["one"]
        (NoArg
            (\cfg -> return cfg { configAllModels = False }))
        "Display one model"

    , Option "" ["tptp-path"]
        (OptArg
            (\arg cfg -> return $ 
                         case arg of
                           Nothing -> cfg
                           Just p  -> cfg { configTPTPPath = p})
            "PATH")
        "Path to TPTP root directory"

    , Option "f" ["input-type"]
        (OptArg
            (\arg cfg -> return $ 
                         case arg of
                           Nothing -> cfg
                           Just f  -> cfg { configInputType = 
                                                case f of
                                                  "geo" -> GeoLog
                                                  "cnf" -> TPTPCNF
                                                  "fof" -> TPTPFOF })
            "geo/cnf/fof")
        "Type of the input formula"

    , Option "" ["iso-elim"]
        (NoArg
            (\cfg -> return cfg { configIsoElim = True }))
        "Eliminate isomorphic models"

    , Option "" ["skolem-depth"]
        (OptArg
            (\arg cfg -> return $ 
                         case join (readMaybe <$> arg) of
                           Nothing -> cfg
                           Just sk -> cfg { configSkolemDepth = sk})
            "#")
        "Depth of skolem term for reusing elements (-1 for pure minimal models)"

    , Option "h" ["help"]
        (NoArg
            (\_ -> do
               prg <- getProgName
               hPutStrLn stderr (usageInfo prg options)
               exitWith ExitSuccess))
        "Show help"

    , Option "v" ["version"]
        (NoArg
            (\_ -> do
               hPutStrLn stderr "Version 3.7"
               exitWith ExitSuccess))
        "Print version"
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
	let inputLines = lines input
	let sequents = mapM (parseFolToSequents False) (filter isRealLine inputLines)
	return $ concat <$> sequents



-- In: configuration, theory
-- Out: G*, which consists of ground facts, provenance info, and a propositional theory
generateGS :: Config -> Theory -> (ChaseHerbrandBaseType, ProvInfo, SATTheoryType)
generateGS config theory = chase config theory
-- In: a propositional theory
-- Out: an iterator that can be used to sequentially generate models (model stream)
modelStream :: SATTheoryType -> SATIteratorType
modelStream propThy = satInitialize propThy
-- In: a model stream
-- Out: an updated model stream and the next model
nextModel :: SATIteratorType -> (Maybe Model, SATIteratorType)
nextModel it = (satSolve it)



-- In: model of a theory, a term representing an element
-- Out: a list of elements this model is equivalent to in the given model
getEqualElements :: Model -> Term -> [Term]
getEqualElements mdl term = case term of
  (Elem e) -> case (Map.lookup e (modelElements mdl)) of
    Nothing -> []
    Just eqelms -> (map (\e->(Elem e)) eqelms)
  _ -> []
-- In: prov of a theory, and a term representing an element
-- Out: the skolem tree if it exists 
getSkolemTrees :: ProvInfo -> Model -> Term -> [(Element, FnSym, [Element])]
getSkolemTrees prov mdl term = do
  -- turn the parsed term into an element
  case (termToElement term) of
    Nothing -> []
    Just elm -> do
      -- get the provenance skolem tree for this element
      case (getElementProv elm (elementProvs prov)) of
        [] -> []
        trees -> case (getSkolemHead (head trees)) of
          Nothing -> []
          Just (skolemhead, skolemrest) -> do
            let actual = (elm, skolemhead, (concatMap (\t->(maybeToList (getSkolemElement prov t))) skolemrest))
            let equal = (map (\(e, h, r)->(elm, h, r)) (concat (map (getSkolemTrees prov mdl) (delete (Elem elm) (getEqualElements mdl (Elem elm))))))
            actual:equal
getSkolemHead :: Term -> Maybe (FnSym, [Term])
getSkolemHead skolemtree = do
  case skolemtree of
    (Fn skolemhead skolemrest) -> Just (skolemhead, skolemrest)
    (Cons (Constant skolemhead)) -> Just (skolemhead, [])
    _ -> Nothing
getSkolemElement :: ProvInfo -> Term -> Maybe Element
getSkolemElement prov skolemterm = (findElementWithProv skolemterm (elementProvs prov))



-- In: element, skolem function name, theory
-- Out: a theory with the associated exists variable in every related sequent replaced by the element
nameTheory :: Theory -> [(Element, FnSym, [Element])] -> Theory
nameTheory thy names = map (nameSequent names) (zip thy (preprocess thy))
-- In: element, skolem function name, sequent in theory
-- Out: a sequent with the associated exists variable in the head replaced by the element
nameSequent :: [(Element, FnSym, [Element])] -> (Sequent, Sequent) -> Sequent
nameSequent names ((Sequent obdy ohd), (Sequent bdy hd)) = do
  let headnames = map (\(e, h, r)->(h, e)) names
  let rests = map (\(e, h, r)->r) names
  let constsubs = Map.fromList (map (\(e, h, r)->((Constant (T.unpack (head (T.splitOn (T.pack "^") (T.pack h))))), (Elem e))) names)
  -- name head constants / functions
  let (Sequent cbdy chd) = Sequent obdy (substituteConstants constsubs ohd)
  -- name head existentially quantified variables
  let (Sequent nbdy nhd) = Sequent cbdy (nameHead headnames chd hd)
  -- name body IFF head constants, functions, or exists were named
  if ((Sequent nbdy nhd) == (Sequent obdy ohd))
    then (Sequent obdy ohd)
    else do
      let frees = (freeVars (Sequent nbdy nhd))
      let elms = map (\e->(Elem e)) (concat rests)
      let freesubs = Map.fromList (zip frees elms)
      (Sequent (substituteConstants constsubs (substitute freesubs nbdy)) (substitute freesubs nhd))
-- In: element, skolem function name, head formula in sequent
-- Out: a formula with the associated exists variable replaced by the element
nameHead :: [(FnSym, Element)] -> Formula -> Formula -> Formula
-- Junctions aka Keep looking
nameHead names (And of1 of2) (And f1 f2) = (And (nameHead names of1 f1) (nameHead names of2 f2))
nameHead names (Or of1 of2) (Or f1 f2) = (Or (nameHead names of1 f1) (nameHead names of2 f2))
-- Exists
nameHead names (Exists ofn ov off) (Exists (Just fn) v ff) = do 
  let skolemMap = Map.fromList names
  case (Map.lookup fn skolemMap) of
    Just elm -> substitute (Map.fromList [(ov, (Elem elm))]) (nameHead names off ff)
    Nothing -> (Exists ofn ov (nameHead names off ff))
-- Everything else aka Nothing to replace
nameHead _ ofml _ = ofml



--
--
getFact :: Model -> Formula -> Maybe (FnSym, [Term])
getFact mdl fml = case fml of
  (Atm (Rel rsym terms)) -> do
    case (toObservation (Rel rsym terms)) of
      Nothing -> Nothing
      Just obv -> if (elem obv (modelObservations mdl))
        then do
          let elms = concat (map (\t->maybeToList (termToElement t)) terms)
          if ((length terms) == (length elms))
            then Just (rsym, terms)
            else Nothing
        else Nothing
  _ -> Nothing
--
--
getBlame :: ProvInfo -> Model -> (FnSym, [Term]) -> [Blame]
getBlame prov mdl (factname, factelms) = do
  let choices = (combBlame (map (getEqualElements mdl) factelms))
  concat (map (\choice->(getObvProvs prov (factname, choice))) choices)

--
--
combBlame :: [[Term]] -> [[Term]]
combBlame [l] = do
  choose <- l
  return (return choose)
combBlame (l:ls) = do
  choose <- l
  rest <- (combBlame ls)
  return (choose : rest)
combBlame [] = []
--
--
getObvProvs :: ProvInfo -> (FnSym, [Term]) -> [Blame]
getObvProvs prov (factname, factelms) = do
  let obv = fromMaybe (Obs (Rel "" [])) (toObservation (Rel factname factelms))
  maybeToList ((findObservationWithProv obv (observationProvs prov)))



--
--
blameTheory :: Theory -> [(Element, FnSym , [Element])] -> [Blame] -> Theory
blameTheory thy names blames = do
  let namedthy = nameTheory thy names
  let blamereplaces = Map.fromList (map (\(TheoryBlame i s)->(i, s)) blames)
  (sequent, namedsequent) <- zip thy namedthy
  newsequent <- case (elemIndex sequent thy) of
    Nothing -> return sequent
    Just i -> case (Map.lookup (i+1) blamereplaces) of
      Nothing -> return sequent
      Just blamesub -> do
        return (substitute blamesub namedsequent)
  return newsequent