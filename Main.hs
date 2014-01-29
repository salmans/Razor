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
      modelLoop $ runChase Nothing formulae
    otherwise -> error "Usage: ./Main filename"

readFormulae :: FilePath -> IO [Sequent]
readFormulae inputFileName = readFile inputFileName >>= return . map parseSequent . filter Utils.Utils.isRealLine . lines

data UserCommand = Up Obs | Next deriving Read

modelLoop :: [Problem] -> IO ()
modelLoop probs = case probs of 
  [] -> putStrLn "No more models found."
  (prob@Problem {problemModel = oldModel, problemLastConstant = oldConst}):rest -> do
    putStrLn "Model:"
    putStrLn $ show oldModel
    putStr "> "
    hFlush stdout
    userInput <- getLine
    if Utils.Utils.isNonEmptyLine userInput then
      modelLoop $ case (read userInput) of
        Up constraint -> let (newModel, _, newConst) = add oldModel oldConst [constraint] $ Just UserProv in
          runChaseWithProblem prob {problemModel = newModel, problemLastConstant = newConst}
        Next -> rest
      else return ()
