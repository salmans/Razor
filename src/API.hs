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
-- Out: the head of the skolem tree if it exists 
getSkolemHead :: ProvInfo -> Term -> Maybe (Element, FnSym)
getSkolemHead prov term = do
  -- turn the parsed term into an element
  case (termToElement term) of
    Nothing -> Nothing
    Just elm -> do
      -- get the provenance skolem tree for this element
      case (getElementProv elm (elementProvs prov)) of
        [] -> Nothing
        skolemtree -> do
          -- get just the head of the skolem tree
          case (head skolemtree) of
            (Fn skolemhead _) -> Just (elm, skolemhead)
            (Cons (Constant skolemhead)) -> Just (elm, skolemhead)
            _ -> Nothing
          
-- In: element, skolem function name, theory
-- Out: a theory with the associated exists variable in every related sequent replaced by the element
nameTheory :: Element -> FnSym -> Theory -> Theory
nameTheory elm skolemFn thy = map (nameSequent elm skolemFn) (zip thy (preprocess thy))
-- In: element, skolem function name, sequent in theory
-- Out: a sequent with the associated exists variable in the head replaced by the element
nameSequent :: Element -> FnSym -> (Sequent, Sequent) -> Sequent
nameSequent elm skolemFn ((Sequent obdy ohd), (Sequent bdy hd)) = (Sequent obdy (nameHead elm skolemFn ohd hd))
-- In: element, skolem function name, head formula in sequent
-- Out: a formula with the associated exists variable replaced by the element
-- TODO; once the proper exists variable has been found, any occurances of the var further in the formula should be replaced as well
nameHead :: Element -> FnSym -> Formula -> Formula -> Formula
nameHead (Element elm) skolemFn (Exists ofn ov off) (Exists (Just fn) v ff) = do 
  case (fn == skolemFn) of
    True -> (Exists ofn (Variable elm) (nameHead (Element elm) skolemFn off ff))
    False -> (Exists ofn ov (nameHead (Element elm) skolemFn off ff))
nameHead (Element elm) skolemFn (Lone ofn ov off ofs) (Lone (Just fn) v ff fs) = do 
  case (fn == skolemFn) of
    True -> (Lone ofn (Variable elm) (nameHead (Element elm) skolemFn off ff) ofs)
    False -> (Lone ofn ov (nameHead (Element elm) skolemFn off ff) ofs)
nameHead elm skolemFn (Exists ofn ov off) (Exists Nothing v ff) = (Exists ofn ov (nameHead elm skolemFn off ff))
nameHead elm skolemFn (Lone ofn ov off ofs) (Lone Nothing v ff fs) = (Lone ofn ov (nameHead elm skolemFn off ff) ofs)
nameHead e sfn ofml fml = ofml