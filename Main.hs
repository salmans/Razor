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

modelLoop :: [Problem] -> IO ()
modelLoop probs = case probs of 
  [] -> putStrLn "Contradiction! You lose."
  (prob@Problem {problemModel = oldModel, problemLastConstant = oldConst}):_ -> do
    putStrLn "Model:"
    putStrLn $ show oldModel
    putStr "Fact> "
    hFlush stdout
    userInput <- getLine
    if Utils.Utils.isNonEmptyLine userInput then
      let (newModel, _, newConst) = add oldModel oldConst [read userInput] $ Just UserProv in
      modelLoop $ runChaseWithProblem prob {problemModel = newModel, problemLastConstant = newConst}
      else return ()
