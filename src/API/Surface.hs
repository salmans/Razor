{-|
  Razor
  Module      : API.Surface
  Description : The module provides a surface API for user level commands interacting with the core API
  Maintainer  : Salman Saghafi, Ryan Danas
-}
module API.Surface where
import API.Core
import Common.Model
import Common.Provenance
import Syntax.GeometricUtils
import SAT.Impl
import Tools.Config
import Data.Either
import Data.Maybe
import System.Environment

type UState = (Theory, ProvInfo, SATIteratorType)

getConfig :: IO Config
getConfig = do 
	args <- getArgs
	parseConfig args
getStartState :: Config -> IO (Either String UState)
getStartState config = do
  case (configInput config) of
    Nothing -> return $ Left "No input file specified"
    Just infile -> do
    	input <- readFile infile
    	thy <- parseTheory config input
    	case thy of
    		Just theory -> do
    			let (base, prov, prop) = generateGS config theory
     			let stream = modelStream prop 
     			return $ Right (theory, prov, stream)
     		Nothing -> return $ Left "Unable to parse input theory!"

--getNextModel :: UState -> Either String (Theory, ProvInfo, SATIteratorType)