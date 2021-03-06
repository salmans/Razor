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

  Module      : Common.IData
  Description : Implements common data types that are used by various layers
  of the program.
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

{-# LANGUAGE Rank2Types #-}

module Common.IData where

-- Standard
import qualified Data.Map as Map

-- Parsec
import qualified Text.ParserCombinators.Parsec as P

-- Syntax
import Syntax.GeometricUtils
import Syntax.FirstOrderParser -- for parsing skolem terms

-- Common
import Common.Basic


{-| Since the input theory is assumed to be flattened and relationalized, every
  'Constant' of the original theory (if shows up in a model) is always equal
  to some 'Element' of the model. Therefore, it is slightly more efficient and 
  the implementation is cleaner to not pass the constants of the original theory
  to the SMT/SAT solver. Rather, we keep a map from constants to the elements
  that represents them. After a model is returned by the SAT/SMT solver, we 
  extend the model with values for constants in the original theory.
-}
type ConstantValueMap = Map.Map Constant Element

{-| The SequentLike type class captures types that represent geometric 
  'Sequent's in 'PossibleFacts'. This allows for different implementations of 
  'PossibleFacts' with different algorithms. 

  [@fromSequent@] creates a SequentLike instance for an input 'Sequent'. 
  [@toSequent@] returns the original 'Sequent' for a 'SequentLike' instance.
  [@skolemFunctions@] returns a list of Skolem functions assigned to the 
  existential quantifiers of an instance of type @s@. Left values are Skolem 
  functions for ordinary existential quantifiers and Right values are Skolem 
  functions for Lone quantifiers. 
  [@failSequent@] returns true if the head of the sequent is empty. 
  [@startSequent@] returns true if the body of the sequent is empty. -}
class (Show s) => SequentLike s where
    fromSequent      :: Sequent -> Maybe s
    toSequent        :: s -> Sequent
    skolemFunctions  :: s -> [Either FnSym (FnSym, Atom)]
    sequentConstants :: s -> [Constant]
    startSequent     :: s -> Bool
    failSequent      :: s -> Bool


{- 'Sequnet' is 'SequentLike' -}
instance SequentLike Sequent where
    toSequent        = id
    fromSequent s     = Just s
    startSequent s   = (sequentBody s) == Tru
    failSequent  s   = (sequentHead s) == Fls
    skolemFunctions  = sequentExistentials
    sequentConstants = constants

{- Custom Skolem depth for a given Skolem depth -}
type SkolemDepth    = (FnSym, Int)

{- SkolemDepthMap is a structure for mapping Skolem term functions -}
type SkolemDepthMap = Map.Map FnSym Int

{- empty SkolemDepthMap -}
emptySkolemDepthMap :: SkolemDepthMap
emptySkolemDepthMap =  Map.empty

{- find in SkolemDepthMap -}
findSkolemDepthWithDefault :: Int -> FnSym -> SkolemDepthMap -> Int
findSkolemDepthWithDefault =  Map.findWithDefault

pSkolemDepthMap :: P.Parser SkolemDepthMap
pSkolemDepthMap = do
  skDepths <- commaSep pSkolemDepth
  return $ Map.fromList skDepths

pSkolemDepth :: P.Parser SkolemDepth
pSkolemDepth  = do
  symbol "@DEPTH"
  sk     <- pSkolemFunction
  symbol "="
  depth  <- natural
  return (sk, fromIntegral depth)