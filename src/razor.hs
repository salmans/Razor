{-# LANGUAGE FlexibleContexts #-}

{-| 
  Razor
  Module      : Main
  Description : This is the primary interface to 'Razor'.
  Maintainer  : Salman Saghafi 
-}

module Main where

-- Standard
import Data.Maybe
import Data.List
import Text.Read (readMaybe)

-- System
import System.Environment
import System.Console.GetOpt
import System.Exit (exitWith, ExitCode (..))
import System.IO (hPutStrLn, stderr)

-- Control
import Control.Monad
import Control.Applicative

-- Common
import Common.Provenance (ProvInfo)
import Common.Model (Model (..), addConstants)
import Chase.Data (baseConstants)

-- Chase
import Chase.Impl

-- SAT
import SAT.Impl

-- Syntax
import Syntax.GeometricUtils ( Theory, Sequent, Constant, Element, parseSequent)

-- Tools
import Tools.Utils (isRealLine)
import Tools.Config
import Tools.Counter
import Tools.FolToGeo (parseFolToSequents)

-- import qualified Codec.TPTP as TP
-- import TPTP.Translate as T2G

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
          case configInputType config of
            GeoLog -> do
                geoFmlas <- geoFormulas inputFileName
                case geoFmlas of
                  Just fs -> return (fs, [])
                  Nothing -> error "The input is not geometric!"
            -- _    -> do
            --     tptpData <- tptpFormulas (configTPTPPath config) inputFileName
            --     case tptpData of
            --       Just (fs, es) -> return (fs, es)
            --       Nothing -> error "The input is not geometric!"
--            _      -> error "input type is not supported!"


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
  runRazor config inputFmlas []

  putStrLn ""


-- Runs the chase according to given parameters
runRazor :: Config -> Theory -> [Constant] -> IO ()
runRazor config fmlas consts =   
    case configInputType config of
      GeoLog -> let (b, provs, propThy) = chase config fmlas
                    cvMap               = Just $ baseConstants b
                    it                  = satInitialize propThy
                    (model', it')       = satSolve it
                    (model'', it'')     = satSolve it'
                    (model''', it''')   = satSolve it''
                    (model'''', _)      = satSolve it'''
                in do                  
                  putStrLn $ show (addConstants <$> model' <*> cvMap)
                  putStrLn $ show (addConstants <$> model'' <*> cvMap)
                  putStrLn $ show (addConstants <$> model''' <*> cvMap)
                  putStrLn $ show (addConstants <$> model'''' <*> cvMap)
      _      -> error "The input type is not currently supported!"
          -- let seqMap = sequentMap fmlas :: SequentMap RelSequent
          -- in  putStrLn 
          --         $ show (chaseWithInitialConstants' config seqMap consts
          --                     :: RelHerbrandBase)

-- Two different functions for loading TPTP and geometric input theories.
geoFormulas :: String -> IO (Maybe Theory)
geoFormulas fName = do
  src <- readFile fName
  let inputLines = lines src
      realLines  = filter isRealLine inputLines
      inputFmlas = mapM (parseFolToSequents False) realLines
  return $ concat <$> inputFmlas

-- tptpFormulas :: String -> String -> IO (Maybe (Theory, [Constant]))
-- tptpFormulas tptpPath fName = do
--     inputs <- tptpLoad tptpPath fName
--     return $ T2G.translate $ concat inputs

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