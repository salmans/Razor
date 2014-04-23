{-| This module is the primary interface to Atlas!
-}


module Main where
import System.Environment
import System.Console.GetOpt
import System.Console.Readline
import System.Exit (exitWith, ExitCode (..))
import System.IO (hPutStrLn, stderr, hFlush, stdout)
import Text.Read (readMaybe)

import Control.Monad
import Control.Applicative

import Data.Maybe
import Data.List
import qualified Data.Map as Map

import Chase.Formula.SyntaxGeo (Theory, Sequent, Term, Elem, parseSequent,
                                Formula, parseCommand, Command(..), ModelExpr(..),
                                ModelOperation(..))
import Chase.Utils.Utils (isRealLine, isNonEmptyLine)
import Chase.Tools.Config
import Chase.Tools.FolToGeo
import qualified Chase.Problem.Model as Model
import Chase.Chase (chase, chase', chaseWithModel, runChase, runChaseWithProblem, deduceForFrame)
import Chase.Problem.Observation
import Chase.Problem.Operations
import Chase.Problem.Provenance
import Chase.Problem.Structures

import qualified Codec.TPTP as TP
import Chase.TPTP.TPTPToGeo as T2G

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
 
{-
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
-}
  modelLoop config Map.empty Nothing

data GraphLoc = GraphLoc Theory Int [(Formula, Int)]

modelLoop :: Config -> Map.Map String GraphLoc -> Maybe GraphLoc -> IO ()
modelLoop config bindings lastLoc = do
  let loop = modelLoop config
      continue = loop bindings lastLoc
  userLine <- readline "> "
  case userLine of
    Nothing -> return ()
    Just userInput -> if Chase.Utils.Utils.isNonEmptyLine userInput then
      addHistory userInput >> case (parseCommand userInput) of
        Nothing -> putStrLn "Syntax error." >> continue
        Just cmd -> case cmd of
          Display expr -> do
            maybeLoc <- resolveModelExpr expr bindings lastLoc
            case maybeLoc of
              Nothing -> putStrLn "Invalid expression." >> continue
              Just loc -> case resolveGraphLoc config loc of
                Nothing -> putStrLn "Model not found." >> continue
                Just (prob,_) -> putStrLn (show $ problemModel prob) >> loop bindings maybeLoc
          Assign var expr -> do
            maybeLoc <- resolveModelExpr expr bindings lastLoc
            case maybeLoc of
              Nothing -> putStrLn "Invalid expression." >> continue
              Just loc -> loop (Map.insert var loc bindings) lastLoc
          Exit -> return ()
      else continue

resolveModelExpr :: ModelExpr -> Map.Map String GraphLoc -> Maybe GraphLoc -> IO (Maybe GraphLoc)
resolveModelExpr expr bindings lastLoc = case expr of
  ThyLiteral thy -> return $ Just $ GraphLoc thy 0 []
  LoadFromFile filename -> do
    maybeThy <- geoFormulas filename
    return $ case maybeThy of
      Nothing -> Nothing
      Just thy -> Just $ GraphLoc thy 0 []
  ApplyOp preExpr op -> do
    maybeLoc <- resolveModelExpr preExpr bindings lastLoc
    return $ case maybeLoc of
      Nothing -> Nothing
      Just (GraphLoc thy initialIndex steps) -> case op of
        AddConstraint aug -> Just $ GraphLoc thy initialIndex (steps ++ [(aug,0)])
        RemoveConstraint -> case steps of
          [] -> Nothing
          _ -> Just $ GraphLoc thy initialIndex $ init steps
        NextModel -> Just $ case steps of
          [] -> GraphLoc thy (succ initialIndex) []
          _ -> let (aug,lastIndex) = last steps in
            GraphLoc thy initialIndex (init steps ++ [(aug,succ lastIndex)])
        PreviousModel -> case steps of
          [] -> case initialIndex of
            0 -> Nothing
            _ -> Just $ GraphLoc thy (pred initialIndex) []
          _ -> let (aug,lastIndex) = last steps in
            case lastIndex of
              0 -> Nothing
              _ -> Just $ GraphLoc thy initialIndex (init steps ++ [(aug,pred lastIndex)])
        FirstModel -> case steps of
          [] -> Just $ GraphLoc thy 0 []
          _ -> let (aug,_) = last steps in
            Just $ GraphLoc thy initialIndex (init steps ++ [(aug,0)])
        Origin -> Just $ GraphLoc thy 0 []
  LastResult -> return lastLoc
  ModelVar var -> return $ Map.lookup var bindings

resolveGraphLoc :: Config -> GraphLoc -> Maybe (Problem, FrameMap)
resolveGraphLoc config (GraphLoc thy initialIndex steps) =
  case steps of
    [] -> let (frms,initialProblem) = buildProblem thy
              stream = runChase config Nothing frms initialProblem in
          if length stream > initialIndex then Just ((stream !! initialIndex),frms) else Nothing
    _ -> case resolveGraphLoc config (GraphLoc thy initialIndex (init steps)) of
      Nothing -> Nothing
      Just (prob@Problem {problemModel = oldModel, problemLastConstant = oldConst},frms) -> 
        let (aug,lastIndex) = last steps
            (preDeducedObs,_) = processHead aug
            ([postDeducedObs],intermediateConst) = deduceForFrame oldConst oldModel preDeducedObs
            (newModel,_,newConst) = Model.add oldModel oldConst postDeducedObs UserProv
            stream = runChaseWithProblem config frms prob {problemModel = newModel, problemLastConstant = newConst} in
        if length stream > lastIndex then Just ((stream !! lastIndex),frms) else Nothing

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
