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
import Text.Read (readMaybe)
import Data.Maybe

import Formula.SyntaxGeo
import Utils.Utils (isRealLine)
import Tools.Config
import Chase.Problem.Model
import Chase.Problem.Structures
import Chase.Chase (chase, chase')

import qualified Data.List as List
import qualified Data.Map as Map

import Chase.Problem.BaseTypes
import Chase.Problem.Provenance
import Chase.Problem.Observation
import Chase.Problem.RelAlg.RelAlg
import qualified Chase.Problem.RelAlg.Operations as OP
import qualified RelAlg.DB as DB
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
                           configSchedule = case arg of
                                              "dfs" -> SchedDFS
                                              "rr"  -> SchedRR
                                              _     -> SchedBFS })
            "bfs/dfs/rr")
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

    , Option "" ["process-unit"]
        (OptArg
            (\arg cfg -> return $
                         case arg of
                           Nothing -> cfg
                           Just pu ->
                               case readMaybe pu of
                                 Nothing  -> cfg
                                 Just pu' -> cfg { configProcessUnit = pu'})
            "# OF BIG STEPS")
        "Process unit for round robing scheduling"

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
               hPutStrLn stderr "Version 3.4"
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
--  putStrLn $ "\n" ++ "===================="
--  putStrLn $ "file: " ++ show inputFileName

  -- echo the input theory:
--  putStrLn "Theory: "
--  mapM_ (putStrLn.show) inputFmlas
--  putStrLn $ "\n" ++ "===================="

--  putStrLn "Input Options: "
--  putStrLn (show config)
--  putStrLn $ "\n" ++ "===================="

--  putStrLn "Models: \n"

  putStrLn "("
  if   configAllModels config
  then putStrLn $ problemsSexp $ chase config inputFmlas
  else putStrLn $ case chase' config inputFmlas of
                    Nothing -> problemsSexp []
                    Just m  -> problemsSexp [m]
  putStrLn ")"


problemsSexp :: [Problem] -> String
problemsSexp []    = ";; No models found!"
problemsSexp probs = List.intercalate "\n\n" (List.map strProblem probs)
  where strProblem p =
          let model = problemModel p in
          " ( ;; Model\n" ++
--          (show model) ++ "\n\n" ++
--          (modelSexp model) ++ "\n\n" ++
          (provSexp (modelProvInfo model)) ++
--        "\n" ++ (framesSexp (problemFrames p)) ++
          "\n )"


modelSexp :: Model -> String
modelSexp model = do
  let list = List.filter (\(tr, _) -> tr /= DomTable) $ Map.toList (modelTables model)
  "\n" --(List.concat (\(tr, t) -> prettyTable tr t) list) ++ "\n"


provSexp :: ProvInfo -> String
provSexp provs =
  "  ( ;; Provenance\n" ++
  (List.intercalate "\n" $
   List.map snd $
   List.sortBy compareFst $
   List.concatMap strFctProv (Map.assocs (provInfoData provs))) ++
  "\n  )"
  where strFctProv ((Fct (R a b)), provl) = strFctProv2 a b provl
        strFctProv ((Fct (F a b)), provl) = strFctProv2 a b provl
        strFctProv ((Eql (Fn a []) e@(Elm _)), prov1) = strFctProv2 a [e] prov1
        --strFctProv ((Den (Fn a [])), provl) = strFctProv2 a [] provl
        --strFctProv a = error (show a)
        --strFctProv _ = []

        strFctProv2 a b provl = List.map (strProv ("   ((" ++ a ++ (strArgs b) ++ ") (")) (List.nubBy eq provl)
        eq p1 p2 = (provKey p1) == (provKey p2)

        strProv str1 prov = (provKey prov, str1 ++ (strp prov) ++ "))")

        strp (ChaseProv sid fid sub) =
          (show sid) ++ " " ++ (show fid) ++ " (" ++ (strSub sub) ++ ")"
        strp UserProv = "?"

        strSub sub = List.intercalate " " $ List.map strSubEach (Map.assocs sub)
        strSubEach (a, (Elm b)) = "(" ++ a ++ " " ++ b ++ ")"
        strSubEach _ = "(_ _)"

        compareFst (key1,_) (key2,_) = compare key1 key2
        provKey (ChaseProv sid _ _) = sid
        provKey UserProv = -1


framesSexp :: [Frame] -> String
framesSexp frames =
  "  ( ;; Theory\n" ++
  (List.intercalate "\n" $ List.map strFrame $ List.sortBy compareByID frames) ++
  "\n  )"
  where strFrame (Frame id body head _ _ _) =
          "   (" ++ (show id) ++ " (" ++ (list body) ++ ") (" ++ (list2 head) ++ "))"

        list2 obsll = "(" ++ (List.intercalate ") (" (List.map list obsll)) ++ ")"
        list obsl = List.intercalate " " (List.concatMap strObs obsl)
        strObs (Fct (R a b)) = [strObs2 a b]
        strObs (Fct (F a b)) = [strObs2 a b]
        strObs _ = []
        strObs2 a b = "(" ++ a ++ " " ++ (strArgs b) ++ ")"

        compareByID f1 f2 = compare (frameID f1) (frameID f2)


strArgs args = List.concatMap ((++) " ") (List.map strArg args)
strArg (Elm e) = e
strArg (Var x) = x
--strArg x = show x
strArg _ = "_"
