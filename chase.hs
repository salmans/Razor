{- 
  Runs chase' on the input theory and returns the first model
  for the theory (if any exists). It also verifies the output
  model against the input theory.
-}


module Main where
import System.Environment
import System.FilePath

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


main :: IO ()
main = do
  -- get the arguments
  args <- getArgs
  let inputFileBaseName =  
          if null args then error "need an input file"
          else head args
      inputFileName = inputFileBaseName
      configArgs    = tail args
  
  let single    = "-one" `elem` configArgs
      debug     = "-debug" `elem` configArgs
      increment = "-incremental" `elem` configArgs
      batch     = "-batch" `elem` configArgs
      schedType = if "-dfs" `elem` configArgs then SchedFILO else SchedFIFO
      config    = defaultConfig { configDebug       = debug
                                , configSchedule    = schedType
                                , configBatch       = batch
                                , configIncremental = increment}
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

--  putStrLn "Models: \n"

  putStrLn "("
  if   single
  then putStrLn $ case chase' config inputFmlas of
                    Nothing -> problemsSexp []
                    Just m  -> problemsSexp [m]
  else putStrLn $ problemsSexp $ chase config inputFmlas
  putStrLn ")"


problemsSexp :: [Problem] -> String
problemsSexp []    = ";; No models found!"
problemsSexp probs = List.intercalate "\n\n" (List.map strProblem probs)
  where strProblem p =
          let model = problemModel p in
          " ( ;; Model\n" ++
--          (show model) ++ "\n\n" ++
--          (modelSexp model) ++ "\n\n" ++
          (provSexp (modelProvInfo model)) ++ "\n" ++
          (framesSexp (problemFrames p)) ++
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
        strFctProv _ = []

        strFctProv2 a b provl = List.map (strProv ("   ((" ++ a ++ " " ++ (strArgs b) ++ ") (")) (List.nubBy eq provl)
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


strArgs args = List.intercalate " " (List.map strArg args)
strArg (Elm e) = e
strArg (Var x) = x
--strArg x = show x
strArg _ = "_"
