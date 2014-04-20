{-
  Author: vpatara

  Computes cores and output results to stdout
  The module is an interface between Atlas and FindCore, which is Atlas's side
  (while FindCoreI for FindCore's side).
-}


module FCAtlas where

import System.Time
import System.IO
import Debug.Trace
import Control.Applicative
import Control.Monad
import Control.DeepSeq
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map

import qualified Data.List as List
import qualified FindCore.FindCoreI as FC
import FindCore.Utils.Trace (showMapping, showModel)
import FindCore.Model.Element (toPairList)

import Formula.SyntaxGeo
import Chase.Problem.Model
import Chase.Problem.BaseTypes

{-
Runs FindCore on these models and collect statictics such as time and sizes
-}
runCoreTests :: [Model] -> Int -> Bool -> Bool -> IO ()
runCoreTests models modelAtMost useSigTest minimalStat = do
  putStrLn $ "\n" ++ "Start evaluating models"
  hFlush stdout
  let evalModels = deepSeqAtMost models modelAtMost
  putStrLn $ "Finished evaluating " ++ (show $ length evalModels) ++ " model(s)"
  hFlush stdout

--  putStrLn "Model: "
--  putStrLn $ show evalModels

  runRetractionList evalModels useSigTest minimalStat

--  putStrLn $ "\nTotal: " ++ (show $ length evalModels) ++ " model(s)\n\n\n"
  putStrLn $ "\nTotal: " ++ (show $ length evalModels) ++ " model(s)\n"


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



runRetractionList :: [(Model, Int)] -> Bool -> Bool -> IO ()
runRetractionList models useSigTest minimalStat = do
  forM_ models (\m -> runRetraction m useSigTest minimalStat)

runRetraction :: (Model, Int) -> Bool -> Bool -> IO ()
runRetraction (model, id) useSigTest minimalStat = do
  let provA = modelProvInfo model
      provFC = provForFC provA
--      retract = FC.coreRetractFromProvenance provFC
      result = FC.coreResultFromProvenance provFC useSigTest
      (mode, core, ret, allRound, retRound, initFactCount, initElmCount,
       coreFactCount, coreElmCount) = result

  if minimalStat
  then putStrLn $ "\n===== Model " ++ (show id) ++ " ====="
  else putStrLn $ "\n\n========================  Model " ++ (show id) ++
                  "  ========================\n" ++ (show model)
  hFlush stdout

  -- Capture the time to find cores
  timeStart <- getClockTime
  timeEnd   <- deepseq result getClockTime

  -- Compute the running time
  let runTime1 = diffClockTimes timeEnd timeStart
      rtPSec1 = (fromIntegral $ tdPicosec runTime1) :: Float
      rtSec1 = (fromIntegral $ tdSec runTime1) :: Float
      runTime = rtSec1 + (rtPSec1 / 1e12)
      runTimeStr = show runTime

  let modeStrMin = "mode: " ++ (show mode)
  let modeStr = if mode == FC.useNormalMode then "normal mode" else "naive mode"

  let sigStrMin = "usesig: " ++ (if useSigTest then "1" else "0")
  let sigStr = if useSigTest then "use sig test" else "no sig test"

  let coreSizeStrMin = "init: " ++ (show initFactCount) ++
                       "/" ++ (show initElmCount) ++ 
                       "\ncore: " ++ (show coreFactCount) ++
                       "/" ++ (show coreElmCount)

  let coreSizeStr = "fact and element counts:" ++
                    "\n - init: " ++ (show initFactCount) ++
                    "/" ++ (show initElmCount) ++
                    "\n - core: " ++ (show coreFactCount) ++
                    "/" ++ (show coreElmCount) ++ "\n"

  let statStrMin = modeStrMin ++ "\n" ++ sigStrMin ++ "\n" ++ coreSizeStrMin ++
                   "\nrounds: " ++ (show retRound) ++ "/" ++ (show allRound) ++
                   "\ntime: " ++ runTimeStr ++ "s"

  let statStr = modeStr ++ "\n" ++ sigStr ++
                "\n\n" ++ coreSizeStr ++ "core:\n" ++ (showModel core) ++
                "\n\nretraction:\n" ++ (showMapping $ toPairList ret) ++
                "\n\nrounds (ret/all) : " ++ (show retRound) ++ "/" ++ (show allRound) ++
                "\n\ntime : " ++ runTimeStr ++ "s"

  if minimalStat
  then putStrLn $ statStrMin
  else putStrLn $ statStr
  
  hFlush stdout



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

        strp (ChaseProv sbid sqid sub) =
          (show sbid) ++ " " ++ (show sqid) ++ " (" ++ (strSub sub) ++ ")"
        strp UserProv = "?"

        strSub sub = List.intercalate " " $ List.map strSubEach (Map.assocs sub)
        strSubEach (a, (Elm (Elem b))) = "(" ++ a ++ " " ++ b ++ ")"
        strSubEach _ = "(_ _)"

        compareFst (key1,_) (key2,_) = compare key1 key2
        provKey (ChaseProv sbid _ _) = sbid
        provKey UserProv = -1

--{-
provForFC :: ProvInfo -> [FC.Provenance]
provForFC provs =
  (List.map snd $
   List.sortBy compareFst $
   List.concatMap strFctProv (Map.assocs (provInfoData provs)))
  where strFctProv ((Fct (R ('@':'E':'x':'i':'s':'t':'s':_) _)), _) = []
        strFctProv ((Fct (R "@Element" _)), _) = []
        strFctProv ((Fct (R a b)), provl) = strFctProv2 a b provl
        strFctProv ((Fct (F a b)), provl) = strFctProv2 a b provl
        strFctProv ((Eql (Fn a []) e@(Elm _)), prov1) = strFctProv2 a [e] prov1
        --strFctProv ((Den (Fn a [])), provl) = strFctProv2 a [] provl
        --strFctProv a = error (show a)
        --strFctProv _ = []

        strFctProv2 a b provl = List.map (strProv (FC.Fact a (strArg <$> b))) (List.nubBy eq provl)
        eq p1 p2 = (provKey p1) == (provKey p2)

        strProv fact prov = (provKey prov, FC.Provenance fact (strp prov))

        strp (ChaseProv sbid sqid sub) = FC.ProvTag sbid sqid (strSub sub)
        strp UserProv =                FC.ProvTag (-1) (-1) []

        strSub sub = List.map strSubEach (Map.assocs sub)
        strSubEach (a, (Elm (Elem b))) = (a, b)
        strSubEach _ = ("?", "?")

        compareFst (key1,_) (key2,_) = compare key1 key2
        provKey (ChaseProv sbid _ _) = sbid
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
