module Main where
import System.Environment
import System.FilePath
import System.Time
import System.IO
import Debug.Trace
import Control.Exception 
import Data.List
import qualified Data.Either as Either
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map

import Formula.SyntaxGeo
import Formula.UserSyntax
import Utils.GeoUtilities
import qualified Utils.Utils
import Tools.GeoUnification
import Chase.Problem.Model
import Chase.Problem.Operations (sequentHolds)
import Chase.Problem.Structures
import Chase.Problem.Provenance
import Chase.Problem.Observation
import Chase.Chase


main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputPath] -> do
      putStrLn "File:"
      putStrLn $ show inputPath
      formulae <- readFormulae inputPath
      putStrLn "Theory:"
      putStrLn $ show formulae
      modelLoop (runChase Nothing formulae) Map.empty
    otherwise -> error "Usage: ./Main filename"

readFormulae :: FilePath -> IO [Sequent]
readFormulae inputFileName = readFile inputFileName >>= return . map parseSequent . filter Utils.Utils.isRealLine . lines

data UserCommand = Add Obs | Next | Load String | Save String deriving Read

modelLoop :: [Problem] -> Map.Map String [Problem] -> IO ()
modelLoop probs varBindings = case probs of
  [] -> putStrLn "No more models found."
  (prob@Problem {problemModel = oldModel, problemLastConstant = oldConst}):rest -> do
    putStrLn "Model:"
    putStrLn $ show oldModel
    putStr "> "
    hFlush stdout
    userInput <- getLine
    if Utils.Utils.isNonEmptyLine userInput then
      case (read userInput) of
        Add constraint -> let (newModel, _, newConst) = add oldModel oldConst [constraint] $ Just UserProv in
          modelLoop (runChaseWithProblem prob {problemModel = newModel, problemLastConstant = newConst}) varBindings
        Next -> modelLoop rest varBindings
        Load var -> case (Map.lookup var varBindings) of
          Just oldProbs -> modelLoop oldProbs varBindings
          Nothing -> putStrLn "Variable not found." >> modelLoop probs varBindings
        Save var -> modelLoop probs $ Map.insert var probs varBindings
      else return ()
