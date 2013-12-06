{- 
  Runs chase' on the input theory and returns the first model
  for the theory (if any exists). It also verifies the output
  model against the input theory.
-}


module Main where
import System.Environment
import System.FilePath

import Formula.SyntaxGeo (Sequent, parseSequent)
import Utils.Utils (isRealLine)
import Chase.Problem.Model
import Chase.Chase (chase, chase')
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

  -- do input file reading
  src <- readFile inputFileName                    

  -- get a list of sequents from the input file
  let inputLines = lines src
      realLines =  filter isRealLine inputLines
      inputFmlas =  map parseSequent realLines

  -- report result
  --
  putStrLn $ "\n" ++ "===================="
  putStrLn $ "file: " ++ show inputFileName

  -- echo the input theory:
  putStrLn "Theory: "
  mapM_ (putStrLn.show) inputFmlas
  putStrLn $ "\n" ++ "===================="

  putStrLn "Models: \n"

  if allModels
  then printModels $ chase inputFmlas
  else putStrLn $ case chase' inputFmlas of
                    Nothing -> "No models found!"
                    Just m  -> show m
  putStrLn ""


printModels :: [Model] -> IO ()
printModels []   = putStrLn "No models found!"
printModels mdls = do
  mapM_ (\m -> do 
           putStrLn (show m)
           putStrLn ("\n" ++ "--------------------")) mdls