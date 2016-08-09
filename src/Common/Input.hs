{- This file is part of Razor.

  Razor is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Razor is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Razor.  If not, see <http://www.gnu.org/licenses/>.

  Module      : Common.IInput
  Description : The module defines Razor's input format and implements a parser
  for parsing the input file.
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

module Common.Input where

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

parseInput :: String -> Either String Input
parseInput input =
  let pResult = P.parse pInput "parsing input" input
  in  case pResult of
        Left err  -> Left (show err)
        Right val -> Right val

pInput :: P.Parser Input
pInput  = do
  thy   <- pTheory
  skMap <- pSkolemDepthMap
  P.eof;
  return $ Input thy skMap
