module Main where
import System.Environment
import System.FilePath
import System.Time
import System.IO
import Debug.Trace
import Control.Exception 
import Control.Monad
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
      case (runChase Nothing formulae) of
        [] -> putStrLn "No models found."
        row@(model:_) -> do
          putStrLn "First model:"
          putStrLn $ show model
          let origin = GraphLoc row 0
          modelLoop Map.empty origin origin
    otherwise -> error "Usage: ./Main filename"

readFormulae :: FilePath -> IO [Sequent]
readFormulae inputFileName = readFile inputFileName >>= return . map parseSequent . filter Utils.Utils.isRealLine . lines

data UserCommand = Show ModelExpr | Store String ModelExpr | Exit deriving Read
data ModelExpr = Origin | Last | ModelVar String | Add ModelExpr Obs | Next ModelExpr deriving Read
data GraphLoc = GraphLoc [Problem] Int

resolve :: ModelExpr -> Map.Map String GraphLoc -> GraphLoc -> GraphLoc -> Maybe GraphLoc
resolve expr bindings origin last = case expr of
  Origin -> Just origin
  Last -> Just last
  ModelVar name -> Map.lookup name bindings
  Add subExpr obs -> do
    GraphLoc row index <- resolve subExpr bindings origin last
    let prob@Problem {problemModel = oldModel, problemLastConstant = oldConst} = row !! index
        (newModel, _, newConst) = add oldModel oldConst [obs] $ Just UserProv
    case (runChaseWithProblem prob {problemModel = newModel, problemLastConstant = newConst}) of
      [] -> Nothing
      newRow -> Just $ GraphLoc newRow 0
  Next subExpr -> do
    GraphLoc row index <- resolve subExpr bindings origin last
    let nextIndex = succ index
    if nextIndex < length row then Just $ GraphLoc row nextIndex else Nothing

modelLoop :: Map.Map String GraphLoc -> GraphLoc -> GraphLoc -> IO ()
modelLoop bindings origin last = do
  let continue = modelLoop bindings origin last
  putStr "> "
  hFlush stdout
  userInput <- getLine
  if Utils.Utils.isNonEmptyLine userInput then
    case (read userInput) of
      Show expr -> case (resolve expr bindings origin last) of
        Just loc@(GraphLoc row index) -> putStrLn (show (problemModel (row !! index))) >> modelLoop bindings origin loc
        Nothing -> putStrLn "Model not found." >> continue
      Store name expr -> case (resolve expr bindings origin last) of
        Just loc -> modelLoop (Map.insert name loc bindings) origin last
        Nothing -> putStrLn "Model not found." >> continue
      Exit -> return ()
    else continue
