{- 
  Runs chase' on the input theory and returns the first model
  for the theory (if any exists). It also verifies the output
  model against the input theory.
-}


module Main where
import System.Environment
import System.Console.GetOpt
import System.Exit (exitWith, ExitCode (..))
import System.IO (hPutStrLn, stderr)
import Data.Maybe

import Formula.SyntaxGeo (Sequent, parseSequent)
import Utils.Utils (isRealLine)
import Tools.Config
import Chase.Problem.Model
import Chase.Chase (chase, chase')
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

    , Option "d" ["debug"]
        (NoArg
            (\cfg -> return cfg { configDebug = True }))
        "Debug mode"

    , Option "s" ["schedule"]
        (ReqArg
            (\arg cfg -> return cfg { 
                           configSchedule = if   arg == "dfs"
                                            then SchedDFS
                                            else SchedBFS })
            "bfs/dfs")
        "Schedule type"

    , Option "b" ["batch"]
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
               hPutStrLn stderr "Version 3.3.3"
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
  
  -- do input file reading
  src <- readFile inputFileName                    

  -- get a list of sequents from the input file
  let inputLines = lines src
      realLines =  filter isRealLine inputLines
      inputFmlas =  map parseSequent realLines

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

  if   configAllModels config
    then printModels $ chase config inputFmlas
    else putStrLn $ case chase' config inputFmlas of
                    Nothing -> "No models found!"
                    Just m  -> show m

  putStrLn ""


printModels :: [Model] -> IO ()
printModels []   = putStrLn "No models found!"
printModels mdls = do
  mapM_ (\m -> do 
           putStrLn (show m)
           putStrLn ("\n" ++ "--------------------")) mdls