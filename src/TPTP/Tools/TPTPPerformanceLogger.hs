{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- 
   Runs the chase on an input TPTP problem and prints the execution time.
-}

module Main where

-- Standard
import Data.Time
import Data.Maybe
import Data.List
import System.Environment
import System.Console.GetOpt
import System.Exit (exitWith, ExitCode (..))
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

-- Control
import Control.Monad
import Control.Applicative
import Control.DeepSeq

-- Syntax
import Syntax.GeometricUtils ( Theory, Sequent, Constant, Element
                             , parseSequent)

-- Common
import Common.Data (SequentMap (..), sequentMap)

-- Chase
import Chase.Data
import Chase.PossibleFacts.RelAlg.PossibleFacts (RelPossibleFacts, RelSequent)
import Chase.Chase (chase, chaseWithInitialConstants)

import qualified Chase.PossibleFacts.RelAlg.DB as DB (Set) 
       -- to define NFData instance
import qualified Chase.PossibleFacts.RelAlg.ILang as RelAlg (TableRef, Tuple)
       -- to define NFData instance

-- Tools
import Tools.Utils (isRealLine)
import Tools.Config
import Tools.Counter
import Tools.FolToGeo (parseFolToSequents)
import Tools.Trace

-- TPTP
import TPTP.Translate as T2G
import qualified Codec.TPTP as TP

-- ============================
-- Main
-- ============================

instance NFData RelAlg.TableRef where -- enable strict evaluation
    rnf m = m `seq` ()

instance NFData RelAlg.Tuple where -- enable strict evaluation
    rnf m = m `seq` ()

instance NFData (DB.Set RelAlg.Tuple) where -- enable strict evaluation
    rnf m = m `seq` ()

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
                         case join (readMaybe <$> arg) of
                           Nothing -> cfg
                           Just b  -> cfg { configBound = Just b})
            "#")
        "Maximum for bounded model-finding"
    , Option "d" ["debug"]
        (NoArg
            (\cfg -> return cfg { configDebug = True }))
        "Debug mode"

    , Option "n" ["incremental"]
        (NoArg
            (\cfg -> return cfg { configIncremental = True }))
        "Enable incremental view maintenance"
 
    , Option "1" ["one"]
        (NoArg
            (\cfg -> return cfg { configAllModels = False }))
        "Display one model"

    , Option "" ["tptp-path"]
        (OptArg
            (\arg cfg -> return $ 
                         case arg of
                           Nothing -> cfg
                           Just p  -> cfg { configTPTPPath = p})
            "PATH")
        "Path to TPTP root directory"

    , Option "f" ["input-type"]
        (OptArg
            (\arg cfg -> return $ 
                         case arg of
                           Nothing -> cfg
                           Just f  -> cfg { configInputType = 
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

    , Option "" ["depth"]
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
               hPutStrLn stderr "Version 3.6"
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
  let (Just (thy, consts)) = T2G.translate (concat tptpInputs)
      seqMap               = sequentMap thy:: SequentMap RelSequent
      model                = 
          case configInputType config of
                TPTPCNF -> (\(h, _, _) -> h)
                           (chaseWithInitialConstants config seqMap consts)
                               :: RelPossibleFacts
                TPTPFOF -> (\(h, _, _) -> h)
                           (chase config seqMap) 
                               :: RelPossibleFacts

  time2 <- model `deepseq` getCurrentTime

  let diffTime = diffUTCTime time2 time1

  putStrLn $ inputFileName ++ "\t" ++ (show diffTime)


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