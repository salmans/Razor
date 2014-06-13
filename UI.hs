{-| This module is the primary user interface library
-}

module Main where
import System.Environment
import System.Console.GetOpt
import System.Exit (exitWith, ExitCode (..))
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

import Control.Monad
import Control.Applicative

import Data.Maybe
import Data.List

import Formula.SyntaxGeo (Theory, Sequent, Term, Elem, parseSequent)
import Utils.Utils (isRealLine)
import Tools.Config
import Tools.Counter
import Tools.FolToGeo
import qualified Chase.Problem.Model as Model
import Chase.Chase (chase, chase', chaseWithModel)

import Chase.Problem.XMLModel
-- HXT Library Modules
import Text.XML.HXT.Arrow.Pickle
import Text.XML.HXT.Core

-- import qualified Codec.TPTP as TP
-- import TPTP.TPTPToGeo as T2G

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
                         case join (readMaybe <$> arg) of
                           Nothing -> cfg
                           Just b  -> cfg { configBound = Just b})
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
                         case join (readMaybe <$> arg) of
                           Nothing -> cfg
                           Just pu -> cfg { configProcessUnit = pu})
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
            _      -> error "input type is not supported!"
            -- _      -> do
            --     tptpData <- tptpFormulas (configTPTPPath config) inputFileName
            --     case tptpData of
            --       Just (fs, es) -> return (fs, es)
            --       Nothing -> error "The input is not geometric!"


  -- report result
  --
  putStrLn $ "\n" ++ "===================="
  putStrLn $ "file: " ++ show inputFileName

  -- echo the input theory:
  putStrLn "Theory: "
  mapM_ (putStrLn.show) inputFmlas
  putStrLn $ "\n" ++ "===================="

  putStrLn "Input Options: "
  putStrLn (show config)
  putStrLn $ "\n" ++ "===================="

  putStrLn "Models: \n"

  -- run the chase (it sends the results to the output)
  runChase config inputFmlas []

  putStrLn ""


-- Runs the chase according to given parameters
runChase :: Config -> Theory -> [Elem] -> IO ()
runChase config fmlas elems =   
    case configFormulaType config of
      GeoLog -> if   configAllModels config
                then printModels $ chase config fmlas
                else putStrLn $ case chase' config fmlas of
                                  Nothing -> "No models found!"
                                  Just m  -> show m
      _       -> 
          let partial = Model.emptyModelWithElems elems
          in  if   configAllModels config
              then printModels $ chaseWithModel config fmlas partial
              else putStrLn $ 
                   case listToMaybe $ chaseWithModel config fmlas partial of
                     Nothing -> "No models found!"
                     Just m  -> show m

-- Sends a list of models to IO
printModels :: [Model.Model] -> IO ()
printModels []   = putStrLn "No models found!"
printModels mdls = do
  mapM_ (\m -> do 
           putStrLn (showPickled [] m)
           putStrLn ("\n" ++ "--------------------")) mdls


-- Two different functions for loading TPTP and geometric input theories.
-- tptpFormulas :: String -> String -> IO (Maybe (Theory, [Elem]))
-- tptpFormulas tptpPath fName = do
--     inputs <- tptpLoad tptpPath fName
--     return $ T2G.inputsToGeo $ concat inputs


geoFormulas :: String -> IO (Maybe Theory)
geoFormulas fName = do
  src <- readFile fName
  let inputLines = lines src
      realLines  = filter isRealLine inputLines
      inputFmlas = mapM (parseFolToSequent False) realLines
  return $ concat <$> inputFmlas

-- Recursively loads the contents of a file and the files included in it, and
-- returns the content of each file as a separate list of inputs.
-- tptpLoad :: String -> String -> IO [[TP.TPTP_Input]]
-- tptpLoad tptpPath fName = do 
--   { src <- readFile fName
--   ; let inputLines = intersperse "\n" (lines src)
--   ; let tptpInputs = (TP.parse.concat) inputLines
--   ; let (incls, fmlas) = partition isTPTPInclude tptpInputs
--   ; rest <- mapM (\i -> tptpLoad tptpPath (tptpIncludePath tptpPath i)) incls
--   ; return (fmlas:concat rest) }

-- isTPTPInclude :: TP.TPTP_Input -> Bool
-- isTPTPInclude (TP.Include _ _) = True
-- isTPTPInclude _                = False

-- tptpIncludePath :: String -> TP.TPTP_Input -> String
-- tptpIncludePath tptpPath (TP.Include p _) = tptpPath ++ p
-- tptpIncludePath _ _                       = ""
