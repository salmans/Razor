{-| This module is the primary interface to Atlas!
-}


module Main where
import System.Environment
import System.Console.GetOpt
import System.Exit (exitWith, ExitCode (..))
import System.IO (hPutStrLn, stderr, hFlush, stdout)
import Text.Read (readMaybe)

import Control.Monad
import Control.Applicative

import Data.Maybe
import Data.List
import qualified Data.Map as Map

import Formula.SyntaxGeo (Theory, Sequent, Term, Elem, parseSequent)
import Utils.Utils (isRealLine, isNonEmptyLine)
import Tools.Config
import Tools.FolToGeo
import qualified Chase.Problem.Model as Model
import Chase.Chase (chase, chase', chaseWithModel, runChase, runChaseWithProblem)
import Chase.Problem.Observation
import Chase.Problem.Operations
import Chase.Problem.Provenance
import Chase.Problem.Structures

import qualified Codec.TPTP as TP
import TPTP.TPTPToGeo as T2G

-- ============================
-- Main
-- ============================


options :: [ OptDescr (Config -> IO Config) ]
options =
    [ Option "i" ["input", "file"]
        (ReqArg
            (\arg cfg -> return cfg { configInput = Just arg })
            "FILE")
        "Input theory file"
    , Option "b" ["bound"]
        (OptArg
            (\arg cfg -> return $
                         case arg of
                           Nothing -> cfg
                           Just b  ->
                               case readMaybe b of
                                 Nothing  -> cfg
                                 Just b'  -> cfg { configBound = Just b'})
            "#")
        "Maximum for bounded model-finding"
    , Option "d" ["debug"]
        (NoArg
            (\cfg -> return cfg { configDebug = True }))
        "Debug mode"

    , Option "s" ["schedule"]
        (ReqArg
            (\arg cfg -> return cfg { 
                           configSchedule = case arg of
                                              "dfs" -> SchedDFS
                                              "rr"  -> SchedRR
                                              _     -> SchedBFS })
            "bfs/dfs/rr")
        "Schedule type"

    , Option "t" ["batch"]
        (NoArg
            (\cfg -> return cfg { configBatch = True }))
        "Enable batch processing"

    , Option "n" ["incremental"]
        (NoArg
            (\cfg -> return cfg { configIncremental = True }))
        "Enable incremental view maintenance"
 
    , Option "1" ["one"]
        (NoArg
            (\cfg -> return cfg { configAllModels = False }))
        "Display one model"

    , Option "" ["process-unit"]
        (OptArg
            (\arg cfg -> return $
                         case arg of
                           Nothing -> cfg
                           Just pu ->
                               case readMaybe pu of
                                 Nothing  -> cfg
                                 Just pu' -> cfg { configProcessUnit = pu'})
            "#")
        "Process unit for round robing scheduling"

    , Option "" ["tptp-path"]
        (OptArg
            (\arg cfg -> return $ 
                         case arg of
                           Nothing -> cfg
                           Just p  -> cfg { configTPTPPath = p})
            "PATH")
        "Path to TPTP root directory"

    , Option "f" ["formula-type"]
        (OptArg
            (\arg cfg -> return $ 
                         case arg of
                           Nothing -> cfg
                           Just f  -> cfg { configFormulaType = 
                                                case f of
                                                  "geo" -> GeoLog
                                                  "cnf" -> TPTPCNF
                                                  "fof" -> TPTPFOF })
            "geo/cnf/fof")
        "Type of the input formula"

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
               hPutStrLn stderr "Version 3.6"
               exitWith ExitSuccess))
        "Print version"
    ]


main :: IO ()
main = do
  -- get the arguments
  args <- getArgs

  -- Parse options, getting a list of option actions
  let (actions, nonOptions, errors) = getOpt RequireOrder options args
 
  -- Here we thread startOptions through all supplied option actions
  config <- foldl (>>=) (return defaultConfig) actions
 
  let inputFileName = if   (isJust.configInput) config
                      then (fromJust.configInput) config
                      else error "No input theory is specified!"
  
  (inputFmlas, initElems) <- 
          case configFormulaType config of
            GeoLog -> do
                geoFmlas <- geoFormulas inputFileName
                case geoFmlas of
                  Just fs -> return (fs, [])
                  Nothing -> error "The input is not geometric!"
            _      -> do
                tptpData <- tptpFormulas (configTPTPPath config) inputFileName
                case tptpData of
                  Just (fs, es) -> return (fs, es)
                  Nothing -> error "The input is not geometric!"


  putStrLn $ "File: " ++ show inputFileName
  putStrLn ""
  putStrLn "Theory:"
  mapM_ (putStrLn.show) inputFmlas
  putStrLn ""
  putStrLn "Input options: "
  putStrLn $ show config
  putStrLn ""
  let (frms, initialProblem) = buildProblem inputFmlas
  case (runChase config Nothing frms initialProblem) of
    [] -> putStrLn "No models found."
    row@(model:_) -> do
      putStrLn "First model:"
      putStrLn $ show model
      putStrLn ""
      let origin = GraphLoc row 0
      modelLoop config frms Map.empty origin origin


data UserCommand = Show ModelExpr | Store String ModelExpr | Exit deriving Read

data ModelExpr = Origin | Last | ModelVar String | Add ModelExpr Obs | Next ModelExpr deriving Read

data GraphLoc = GraphLoc [Problem] Int


resolve :: Config -> FrameMap -> ModelExpr -> Map.Map String GraphLoc -> GraphLoc -> GraphLoc -> Maybe GraphLoc
resolve config frms expr bindings origin last = let resolv = resolve config frms in
  case expr of
    Origin -> Just origin
    Last -> Just last
    ModelVar name -> Map.lookup name bindings
    Add subExpr obs -> do
      GraphLoc row index <- resolv subExpr bindings origin last
      let prob@Problem {problemModel = oldModel, problemLastConstant = oldConst} = row !! index
          (newModel, _, newConst) = Model.add oldModel oldConst [obs] UserProv
      case (runChaseWithProblem config frms prob {problemModel = newModel, problemLastConstant = newConst}) of
        [] -> Nothing
        newRow -> Just $ GraphLoc newRow 0
    Next subExpr -> do
      GraphLoc row index <- resolv subExpr bindings origin last
      let nextIndex = succ index
      if nextIndex < length row then Just $ GraphLoc row nextIndex else Nothing


modelLoop :: Config -> FrameMap -> Map.Map String GraphLoc -> GraphLoc -> GraphLoc -> IO ()
modelLoop config frms bindings origin last = do
  let loop = modelLoop config frms
      continue = loop bindings origin last
      resolv = resolve config frms
  putStr "> "
  hFlush stdout
  userInput <- getLine
  if Utils.Utils.isNonEmptyLine userInput then
    case (read userInput) of
      Show expr -> case (resolv expr bindings origin last) of
        Just loc@(GraphLoc row index) -> putStrLn (show (problemModel (row !! index))) >> loop bindings origin loc
        Nothing -> putStrLn "Model not found." >> continue
      Store name expr -> case (resolv expr bindings origin last) of
        Just loc -> loop (Map.insert name loc bindings) origin last
        Nothing -> putStrLn "Model not found." >> continue
      Exit -> return ()
    else continue


-- Two different functions for loading TPTP and geometric input theories.
tptpFormulas :: String -> String -> IO (Maybe (Theory, [Elem]))
tptpFormulas tptpPath fName = do
    inputs <- tptpLoad tptpPath fName
    return $ T2G.inputsToGeo $ concat inputs


geoFormulas :: String -> IO (Maybe Theory)
geoFormulas fName = do
  src <- readFile fName
  let inputLines = lines src
      realLines  = filter isRealLine inputLines
      inputFmlas = mapM (parseFolToSequent False) realLines
  return $ concat <$> inputFmlas

-- Recursively loads the contents of a file and the files included in it, and
-- returns the content of each file as a separate list of inputs.
tptpLoad :: String -> String -> IO [[TP.TPTP_Input]]
tptpLoad tptpPath fName = do 
  { src <- readFile fName
  ; let inputLines = intersperse "\n" (lines src)
  ; let tptpInputs = (TP.parse.concat) inputLines
  ; let (incls, fmlas) = partition isTPTPInclude tptpInputs
  ; rest <- mapM (\i -> tptpLoad tptpPath (tptpIncludePath tptpPath i)) incls
  ; return (fmlas:concat rest) }

isTPTPInclude :: TP.TPTP_Input -> Bool
isTPTPInclude (TP.Include _ _) = True
isTPTPInclude _                = False

tptpIncludePath :: String -> TP.TPTP_Input -> String
tptpIncludePath tptpPath (TP.Include p _) = tptpPath ++ p
tptpIncludePath _ _                       = ""
