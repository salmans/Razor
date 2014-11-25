{-| 
  Razor
  Module      : Common.IInputParser
  Description : The module defines Razor's input format and implements a parser
  for parsing the input file.
  Maintainer  : Salman Saghafi -}
module Common.IInput where

-- Standard
import qualified Data.Map as Map

-- Parsec
import qualified Text.ParserCombinators.Parsec as P

-- Syntax
import Syntax.Term (pTerm)
import Syntax.Geometric (Theory, pTheory)

-- Common
import Common.Data ( SkolemDepthMap, pSkolemDepthMap )

data Input = Input { inputTheory         :: Theory
                   , inputSkolemDepthMap :: SkolemDepthMap }

parseInput :: String -> Input
parseInput input =
  let pResult = P.parse pInput "parsing input" input
  in  case pResult of
        Left err  -> error (show err)
        Right val -> val

pInput :: P.Parser Input
pInput  = do
  thy   <- pTheory
  skMap <- pSkolemDepthMap
  P.eof;
  return $ Input thy skMap