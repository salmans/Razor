{- Time-stamp: <2013-05-14 12:59:51 Salman Saghafi>
  Runs chase' on the input theory and returns the first model
  for the theory (if any exists). It also verifies the output
  model against the input theory.
-}


module Main where
import System.Environment
import System.FilePath
import System.Time
import Debug.Trace
import Control.Exception 
import Control.Applicative
import Control.Monad
import Control.DeepSeq
import qualified Control.Monad.State as State
import Data.List
import qualified Data.Either as Either
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map

import qualified Data.List as List
import qualified FindCore.FindCoreI as FC
import FindCore.Utils.Trace (showMapping)

import Formula.SyntaxGeo
import Utils.GeoUtilities
import Tools.Counter
import Tools.FolToGeo
import qualified Utils.Utils
import Tools.GeoUnification
import Tools.Config
import Chase.Problem.Model
import Chase.Problem.BaseTypes
import Chase.Problem.Operations (sequentHolds)
import Chase.Chase
-- ============================
-- Main
-- ============================


main :: IO ()
main = do
  -- get the arguments
  args <- getArgs                                        :: IO [String]
  let inputFileBaseName =  
          if null args then error "need an input file"
          else head args                     :: String
      inputFileName = inputFileBaseName :: FilePath 

      modelAtMost   = if (length args) > 1
                      then read (args !! 1)
                      else 1

      maxIterations = -1 {-
                      if (length args) > 1
                      then read (args !! 1)
                      else -1 -}

  -- do input file reading
  src <- readFile inputFileName                    

  -- get a list of sequents from the input file
  let inputLines = lines src :: [String]
      realLines =  filter Utils.Utils.isRealLine inputLines     :: [String]
      inputFmlas = case mapM (parseFolToSequent False) realLines of
                     Nothing    -> error "The input is not geometric!" 
                     Just fmlas -> concat fmlas

--  let temp = deepseq inputFmlas ()

--  time1 <- getClockTime
  -- get the answers
  let models  = chase defaultConfig {configSchedule    = SchedBFS
                                    ,configIsoElim     = True
                                    ,configSkolemDepth = -1
                                    ,configIncremental = True } inputFmlas

--  time2 <- model `deepseq` getClockTime
--  let diffTime = diffClockTimes time2 time1
--  putStrLn $ inputFileName ++ "\t" ++ (timeDiffToString diffTime)

  -- Remove functions from the input sequents, like what the chase does:
  let inputFmlas' = renameVars <$> relConvert inputFmlas

  -- verifyAll model inputFmlas'

  -- Verify that every formula in the theory is true:
{-
  let verifyMsg = 
          if Maybe.isJust model
          then (let mdl@(Model trs _ _) = Maybe.fromJust model 
                    domain = Elm <$> modelDomain mdl
                    maps f = Utils.Utils.allMaps (freeVars f) domain
                    insts = (\f -> map 
                                   (\s -> (liftTerm.liftSub) s f) 
                                   (maps f)) 
                    fmlas = concatMap insts inputFmlas'
                in
                (case all (\s -> (sequentHolds (Maybe.fromJust model) s)) 
                          fmlas of
                True -> "Verified!"
                False -> "Oops! The model does not satisfy the theory." ++
                         (show (filter (\s -> (not(sequentHolds (Maybe.fromJust model) s))) 
                          fmlas))
                ))
          else "Nothing to verify!"
-}

  putStrLn "Start evaluating models"
  let evalModels = deepSeqAtMost models modelAtMost
  putStrLn $ "Finished evaluating " ++ (show $ length evalModels) ++ " model(s)"


  -- report result
  --
  putStrLn $ "\n" ++ "===================="
  putStrLn $ "file: " ++ show inputFileName
  -- echo the input theory:
  putStrLn "Theory: "
  putStrLn $ show inputFmlas'

--  putStrLn "Model: "
--  putStrLn $ show evalModels

--  putStrLn ""
--  putStrLn $ "-> " ++ verifyMsg
--  putStrLn ""

  runRetractionList evalModels

  putStrLn $ "\nTotal: " ++ (show $ length evalModels) ++ " model(s)"


{-
Retrieve at most "atMost" models and strictly evaluate them,
assigning model number starting from 1
-}
deepSeqAtMost :: [Model] -> Int -> [(Model, Int)]
deepSeqAtMost models atMost = deepSeqAtMostHelper models atMost 1

deepSeqAtMostHelper :: [Model] -> Int -> Int -> [(Model, Int)]
deepSeqAtMostHelper _ 0 _ = [] -- must check for atMost == 0 before empty list
deepSeqAtMostHelper [] _ _ = []
deepSeqAtMostHelper (model:modelList) atMost counter =
  ((deepseq model model), counter):
    (deepSeqAtMostHelper modelList (atMost - 1) (counter + 1))



runRetractionList :: [(Model, Int)] -> IO ()
runRetractionList models = do
  forM_ models runRetraction

runRetraction :: (Model, Int) -> IO ()
runRetraction (model, id) = do
  let provA = modelProvInfo model
      provFC = provForFC provA
      retract = FC.coreRetractFromProvenance provFC

  -- Capture the time to find cores
  timeStart <- getClockTime
  timeEnd   <- deepseq retract getClockTime

  let runTime1 = timeDiffToString $ diffClockTimes timeEnd timeStart
  let runTime = if runTime1 == "" then "~0 sec" else runTime1

  putStrLn $ "\n\n========================  Model " ++ (show id) ++
             "  ========================\n" ++ (show model)
  putStrLn $ "Retraction:\n" ++ (showMapping retract)
  putStrLn $ "\nTime : " ++ runTime




-- QUICK FIX
-- If a variable name is both free and bound in a sequent, rename the bound
-- variables. For the sake of simplicity, we rename all bound variables 
-- (i.e., existential vairables on right)
-- REMARK: This functions assumes normalized sequents:
renameVars :: Sequent -> Sequent
renameVars seq@(Sequent bdy hd) = 
    Sequent bdy (State.evalState (renameHeadVars hd) 0)

-- Salman: all inductions on formula can be done only once.
renameHeadVars :: Formula -> Counter Formula
renameHeadVars (Or f1 f2) = do
  f1' <- renameHeadVars f1
  f2' <- renameHeadVars f2
  return $ Or f1' f2'
renameHeadVars (And f1 f2) = do
  f1' <- renameHeadVars f1
  f2' <- renameHeadVars f2
  return $ And f1' f2'
renameHeadVars (Exists x f) = do
  x' <- freshSymbol "@bv"
  f' <- renameHeadVars f
  let sub = Map.singleton x (Var x')
  return $ Exists x' ((liftTerm.liftSub) sub f')
renameHeadVars f = return f  --otherwise


-- TO BE USED!
verifyAll mdls inputFmlas = 
    case mdls of
      []    -> print "Nothing to verify!"
      mdls' -> mapM_ (verify inputFmlas) mdls'

-- mapM_ $ verify inputFmlas <$> mdls'

verify inputFmlas mdl@(Model trs _ _) = 
    traceShow "."
    $
    let domain = Elm <$> modelDomain mdl
        maps f = Utils.Utils.allMaps (freeVars f) domain
        fmlas = concatMap insts inputFmlas
        insts = (\f -> map 
                       (\s -> (liftTerm.liftSub) s f) 
                       (maps f)) 
    in (case all (\s -> (sequentHolds mdl s)) fmlas of
          True  -> print "Verified!"
          False -> print $ 
                   "Oops! The model does not satisfy the theory." ++
                  (show (filter (\s -> (not(sequentHolds mdl s))) 
                         fmlas)))




{-
Converts provenance information into the pretty S-expression format
-}
provSexp :: ProvInfo -> String
provSexp provs =
  "  ( ;; Provenance\n" ++
  (List.intercalate "\n" $
   List.map snd $
   List.sortBy compareFst $
   List.concatMap strFctProv (Map.assocs (provInfoData provs))) ++
  "\n  )"
  where strFctProv ((Fct (R ('@':'E':'x':'i':'s':'t':'s':_) _)), _) = []
        strFctProv ((Fct (R a b)), provl) = strFctProv2 a b provl
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
        strSubEach (a, (Elm (Elem b))) = "(" ++ a ++ " " ++ b ++ ")"
        strSubEach _ = "(_ _)"

        compareFst (key1,_) (key2,_) = compare key1 key2
        provKey (ChaseProv sid _ _) = sid
        provKey UserProv = -1

--{-
provForFC :: ProvInfo -> [FC.Provenance]
provForFC provs =
  (List.map snd $
   List.sortBy compareFst $
   List.concatMap strFctProv (Map.assocs (provInfoData provs)))
  where strFctProv ((Fct (R ('@':'E':'x':'i':'s':'t':'s':_) _)), _) = []
        strFctProv ((Fct (R a b)), provl) = strFctProv2 a b provl
        strFctProv ((Fct (F a b)), provl) = strFctProv2 a b provl
        strFctProv ((Eql (Fn a []) e@(Elm _)), prov1) = strFctProv2 a [e] prov1
        --strFctProv ((Den (Fn a [])), provl) = strFctProv2 a [] provl
        --strFctProv a = error (show a)
        --strFctProv _ = []

        strFctProv2 a b provl = List.map (strProv (FC.Fact a (strArg <$> b))) (List.nubBy eq provl)
        eq p1 p2 = (provKey p1) == (provKey p2)

        strProv fact prov = (provKey prov, FC.Provenance fact (strp prov))

        strp (ChaseProv sid fid sub) = FC.ProvTag sid fid (strSub sub)
        strp UserProv =                FC.ProvTag (-1) (-1) []

        strSub sub = List.map strSubEach (Map.assocs sub)
        strSubEach (a, (Elm (Elem b))) = (a, b)
        strSubEach _ = ("?", "?")

        compareFst (key1,_) (key2,_) = compare key1 key2
        provKey (ChaseProv sid _ _) = sid
        provKey UserProv = -1
--}

provenance1 = [FC.Provenance (FC.Fact "P" ["e#1"]) (FC.ProvTag 0 1 []),
               FC.Provenance (FC.Fact "Q" ["e#3"]) (FC.ProvTag 1 2 []),
               FC.Provenance (FC.Fact "Q" ["e#1"]) (FC.ProvTag 2 3 [("x","e#1")]),
               FC.Provenance (FC.Fact "R" ["e#3","e#5"]) (FC.ProvTag 3 4 [("x","e#3")]),
               FC.Provenance (FC.Fact "R" ["e#1","e#7"]) (FC.ProvTag 4 4 [("x","e#1")])]


{-
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
-}

strArgs args = List.concatMap ((++) " ") (List.map strArg args)
strArg (Elm (Elem e)) = e
strArg (Var x) = x
--strArg (Fn ('a':'@':x) []) = 'e':'#':x
--strArg x = show x
strArg _ = "?"
