{-|
  Razor
  Module      : API
  Description : The module provides a stateless API for (G)UI applications to
  				use for interacting with the functional core of Razor
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
import Common.Model (Model (..))
import Control.Applicative
import Control.Monad
import Data.Maybe
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

-- In: prov of a theory, and a term representing an element
-- Out: the skolem tree if it exists 
getSkolemTree :: ProvInfo -> Term -> Maybe (Element, FnSym, [Term])
getSkolemTree prov term = do
  -- turn the parsed term into an element
  case (termToElement term) of
    Nothing -> Nothing
    Just elm -> do
      -- get the provenance skolem tree for this element
      case (getElementProv elm (elementProvs prov)) of
        [] -> Nothing
        trees -> case (getSkolemHead (head trees)) of
          Nothing -> Nothing
          Just (skolemhead, skolemrest) -> Just (elm, skolemhead, skolemrest)
getSkolemHead :: Term -> Maybe (FnSym, [Term])
getSkolemHead skolemtree = do
  case skolemtree of
    (Fn skolemhead skolemrest) -> Just (skolemhead, skolemrest)
    (Cons (Constant skolemhead)) -> Just (skolemhead, [])
    _ -> Nothing
getSkolemElement :: ProvInfo -> Term -> Element
getSkolemElement prov skolemterm = case (findElementWithProv skolemterm (elementProvs prov)) of
  Just elm -> elm
  Nothing -> (Element "")

-- In: element, skolem function name, theory
-- Out: a theory with the associated exists variable in every related sequent replaced by the element
nameTheory :: Element -> FnSym -> [Element] -> Theory -> Theory
nameTheory elm skolemhead skolemrest thy = map (nameSequent elm skolemhead skolemrest) (zip thy (preprocess thy))
-- In: element, skolem function name, sequent in theory
-- Out: a sequent with the associated exists variable in the head replaced by the element
nameSequent :: Element -> FnSym -> [Element] -> (Sequent, Sequent) -> Sequent
nameSequent elm skolemhead skolemrest ((Sequent obdy ohd), (Sequent bdy hd)) = do
  let (Sequent nbdy nhd) = Sequent obdy (nameHead elm skolemhead ohd hd)
  let frees = map (\v->(Var v)) (freeVars (Sequent bdy hd))
  let elms = map (\(Element e)->e) skolemrest
  let replacements = zip frees elms
  if ((Sequent nbdy nhd) == (Sequent obdy ohd))
    then (Sequent obdy ohd)
    else (Sequent (nameTerms replacements nbdy) (nameTerms replacements nhd))
-- In: element, skolem function name, head formula in sequent
-- Out: a formula with the associated exists variable replaced by the element
nameHead :: Element -> FnSym -> Formula -> Formula -> Formula
-- Junctions
nameHead elm skolemFn (And of1 of2) (And f1 f2) = (And (nameHead elm skolemFn of1 f1) (nameHead elm skolemFn of2 f2))
nameHead elm skolemFn (Or of1 of2) (Or f1 f2) = (Or (nameHead elm skolemFn of1 f1) (nameHead elm skolemFn of2 f2))
-- Exists
nameHead (Element elm) skolemFn (Exists ofn ov off) (Exists (Just fn) v ff) = do 
  case (fn == skolemFn) of
    True -> nameTerms [((Var ov), elm)] off
    False -> (Exists ofn ov (nameHead (Element elm) skolemFn off ff))
-- Lone
nameHead (Element elm) skolemFn ofml (Lone fn v ff fs) = do
  case (lookupLone skolemFn (Lone fn v ff fs)) of
    Just (Variable var) -> nameTerms [((Cons (Constant (T.unpack (head (T.splitOn (T.pack "^") (T.pack skolemFn)))))), elm)] ofml
    Nothing -> ofml
-- Everything else
nameHead elm skolemFn ofml fml = ofml
-- In:
-- Out:
lookupLone :: FnSym -> Formula -> Maybe Variable
lookupLone skolemFn (Lone (Just fn) v ff fs) = do
  case (fn == skolemFn) of
    True -> Just v
    False -> (lookupLone skolemFn ff)
lookupLone skolemFn (Lone fn v ff fs) = (lookupLone skolemFn ff)
lookupLone skolemFn fml = Nothing
-- In:
-- Out:
nameTerms :: [(Term, Sym)] -> Formula -> Formula
-- Junctions
nameTerms replacements (And f1 f2) = (And (nameTerms replacements f1) (nameTerms replacements f2))
nameTerms replacements (Or f1 f2) = (Or (nameTerms replacements f1) (nameTerms replacements f2))
-- Exists
nameTerms replacements (Exists fn v f) = (Exists fn v (nameTerms replacements f))
-- Atoms
nameTerms replacements (Atm (Rel rsym terms)) = (Atm (Rel rsym (map (nameTerm replacements) terms)))
nameTerms replacements (Atm (FnRel fsym terms)) = (Atm (FnRel fsym (map (nameTerm replacements) terms)))
-- Everything else
nameTerms replacements fml = fml
-- In: element to replace variable, the variable, term
-- Out: input term with the replacement of variable names
nameTerm :: [(Term, Sym)] -> Term -> Term
nameTerm replacements (Fn fnsym terms) = case (lookup (Cons (Constant fnsym)) replacements) of
  Nothing -> (Fn fnsym terms)
  Just newname -> (Var (Variable newname))
nameTerm replacements term = case (lookup term replacements) of
  Nothing -> term
  Just newname -> (Var (Variable newname))
