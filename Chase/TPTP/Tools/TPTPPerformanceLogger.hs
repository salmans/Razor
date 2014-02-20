{- 
   Runs the chase on an input TPTP problem and prints the execution time.
-}


module Main where
import System.Environment
import System.FilePath
import System.Console.GetOpt
import System.Exit (exitWith, ExitCode (..))
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

import Data.Time
import Text.Printf

import Data.Maybe
import Data.List
import Control.DeepSeq

import Chase.Formula.SyntaxGeo (Term (Elm))
import Chase.TPTP.TPTPToGeo as T2G
import Codec.TPTP as TP

import System.FilePath.Posix
import System.Directory

import Chase.Chase
import qualified Chase.Problem.Model as Model
import Chase.Tools.Config

-- ============================
-- Main
-- ============================

-- The path to the TPTP library is hard-coded for now:

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
               hPutStrLn stderr "Version 3.5"
               exitWith ExitSuccess))
        "Print version"
    ]


main :: IO ()
main = do
  -- get the arguments
  args <- getArgs

  -- Parse options, getting a list of option actions
  let (actions, nonOptions, errors) = getOpt Permute options args
 
  -- Here we thread startOptions through all supplied option actions
  config <- foldl (>>=) (return defaultConfig) actions
 
  let inputFileName = if   (isJust.configInput) config
                      then (fromJust.configInput) config
                      else error "No input theory is specified!"

  time1 <- getCurrentTime

  tptpInputs <- recursiveLoad (configTPTPPath config) inputFileName


  -- 
  let (Just (thy, consts)) = T2G.inputsToGeo (concat tptpInputs)
    
  let model = 
          case configFormulaType config of
                TPTPCNF -> let partial = Model.emptyModelWithElems consts
                           in  listToMaybe $ chaseWithModel config thy partial
                TPTPFOF -> chase' defaultConfig thy
  --

  time2 <- model`deepseq` getCurrentTime

  let diffTime = diffUTCTime time2 time1
  let result = case model of
                 Nothing -> "Unsat"
                 Just _  -> "Sat"

  putStrLn $ inputFileName ++ "\t" ++ result ++ "\t" ++ (show diffTime)


-- Recursively loads the contents of a file and the files included in it, and
-- returns the content of each file as a separate list of inputs.
recursiveLoad :: String -> String -> IO [[TP.TPTP_Input]]
recursiveLoad tptpPath fName = do 
  { src <- readFile fName
  ; let inputLines = intersperse "\n" (lines src)
  ; let tptpInputs = (TP.parse.concat) inputLines
  ; let (incls, fmlas) = partition isInclude tptpInputs
  ; rest <- mapM (\i -> recursiveLoad tptpPath (includePath tptpPath i)) incls
  ; return (fmlas:concat rest) }

isInclude :: TP.TPTP_Input -> Bool
isInclude (TP.Include _ _) = True
isInclude _                = False

includePath :: String -> TP.TPTP_Input -> String
includePath tptpPath (TP.Include p _) = tptpPath ++ p
includePath _ _                       = ""