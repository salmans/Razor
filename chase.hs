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
  
  let allModels = "-all" `elem` (tail args)
      debug     = "-debug" `elem` (tail args)
      schedType = if   "-filo" `elem` (tail args)
                  then SchedFILO else SchedFIFO
      config    = defaultConfig { configDebug = debug
                                , configSchedule = schedType}
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
  if   allModels
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
  (List.intercalate "\n" $ List.map strFctProv (Map.assocs provs)) ++
  "\n  )"
  where strFctProv ((Fct (R a b)), prov) = strFctProv2 a b prov
        strFctProv ((Fct (F a b)), prov) = strFctProv2 a b prov
        strFctProv _ = ""
        strFctProv2 a b prov =
          "   ((" ++ a ++ " " ++ (strArgs b) ++ ") " ++
          "(" ++ (strProv prov) ++ "))"

        strProv prov = List.intercalate " " (List.map strp prov)
        strp (ChaseProv (id, sub)) = (show id) ++ " (" ++ (strSub sub) ++ ")"
        strp UserProv = "?"
        strSub sub = List.intercalate " " $ List.map strSubEach (Map.assocs sub)
        strSubEach (a, (Elm b)) = "(" ++ a ++ " " ++ b ++ ")"
        strSubEach _ = "(_ _)"


framesSexp :: [Frame] -> String
framesSexp frames =
  "  ( ;; Theory\n" ++
  (List.intercalate "\n" $ (List.map strFrame frames)) ++
  "\n  )"
  where strFrame (Frame id body head _ _) =
          "   (" ++ (show id) ++ " (" ++ (list body) ++ ") (" ++ (list2 head) ++ "))"

        list2 obsll = "(" ++ (List.intercalate ") (" (List.map list obsll)) ++ ")"
        list obsl = List.intercalate " " (List.concatMap strObs obsl)
        strObs (Fct (R a b)) = [strObs2 a b]
        strObs (Fct (F a b)) = [strObs2 a b]
        strObs _ = []
        strObs2 a b = "(" ++ a ++ " " ++ (strArgs b) ++ ")"


strArgs args = List.intercalate " " (List.map strArg args)
strArg (Elm e) = e
strArg (Var x) = x
--strArg x = show x
strArg _ = "_"
