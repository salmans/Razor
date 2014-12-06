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

  Module      : Common.IProvenance
  Description : The module defines provenance information for elements and facts
  of a model and provides functions to read and modify it.
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

module Common.IProvenance where

-- Standard
import qualified Data.Map as Map
import Data.Maybe

-- import Data.Hashable
-- Control
import Control.Applicative

-- Syntax
import Syntax.Term ( Term (..), SkolemTerm, Element (..), Constant (..)
                   , Sub, FnSym)

-- Common
import Common.Basic (Id)
import Common.Observation (Observation)

unitName               = "Common.Provenance"
error_missingProv      = "provenance information for element is missing"

{-| Provenance of elements that are created by existential quantifiers are 
  stored as an ElemHistory structure inside a model. 
  Because we need to search in 'ElementProvs' in both directions, we maintain
  a pair of maps from elements to the provenance terms and from the provenance 
  terms to elements. -}
type ElementProvs = ( Map.Map Element SkolemTerm
                    , Map.Map SkolemTerm Element)

{-| Blaming information for an 'Observation' is a sequent of the theory (or the
  extended theory after augmentation), which is identified by its 'Id', together
  with a binding from its free variables to elements of the model. -}
data Blame = TheoryBlame Id Sub | UserBlame Observation
           deriving (Show, Eq, Ord)


{- A basic hash function to work with IHashSet as the underlying database. -}
-- THIS IS A TEMPORARY IMPLEMENTATION. THE FUNCTION HAS TO BE REWRITTEN.
-- instance Hashable Blame where
--     s `hashWithSalt` _ = s

{-| Provenance information for observations is a map from 'Observation's to 
  'Blame'. -}
type ObservationProvs = Map.Map Observation Blame 

{-| ProvInfo contains provenance information for elements and facts. -}
data ProvInfo = ProvInfo { elementProvs     :: ElementProvs
                         , observationProvs :: ObservationProvs
                         } deriving (Show, Eq)

{-| Modifies the provenance information for elements given an update function.-}
modifyElementProvs :: (ElementProvs -> ElementProvs) -> ProvInfo -> ProvInfo
modifyElementProvs f provs = 
    let ps = elementProvs provs
    in  provs { elementProvs = f ps }

{-| Empty provenance information -}
emptyProvInfo :: ProvInfo
emptyProvInfo =  ProvInfo (Map.empty, Map.empty) Map.empty

{-| Empty provenance information with initial provenance for given constants -}
emptyProvInfoWithConstants :: [Constant] -> ProvInfo
emptyProvInfoWithConstants consts = undefined
    -- let elms = (\(Constant c) -> Element c) <$> consts
    --     cs   = Cons <$> consts
    -- in  ProvInfo ( Map.fromList $ zip elms (pure <$> cs)
    --              , Map.fromList $ zip cs elms )
    --              Map.empty


{-| Returns the skolem term, i.e., the provenance information, for the input
  element. -}
getElementProv :: Element -> ElementProvs -> Maybe SkolemTerm
getElementProv elm (provs, _) = Map.lookup elm provs

{-| Adds provenance information for the input 'Element' where the input skolem 
  function has been assigned to the existential quantifier that creates the
  element and the input list of elements are the parameters of the Skolem 
  function. -}
addElementProv :: Element -> FnSym -> [Element] -> ElementProvs -> 
                  ElementProvs
addElementProv elm fn elms provs = 
    let terms  = (flip getElementProv) provs <$> elms 
    in  if   any isNothing terms
        then error $ unitName ++ ".addElementProv: " ++ error_missingProv
        else insertProv elm (if   null elms 
                             then Cons (Constant fn)
                             else Fn fn $ fromJust <$> terms) provs

insertProv :: Element -> SkolemTerm -> ElementProvs -> ElementProvs
insertProv elm term (elemProvs, provElems) =
    let provElems' = Map.insert term elm provElems
    in  (Map.insertWith (flip const) elm term elemProvs, provElems')


{-| Adds provenance information for the input 'Element' where the input skolem 
  function has been assigned to the existential quantifier that creates the
  element and the input list of elements are the parameters of the Skolem 
  function. -}
findElementWithProv :: SkolemTerm -> ElementProvs -> Maybe Element
findElementWithProv term (_, provs) =  Map.lookup term provs

findObservationWithProv :: Observation -> ObservationProvs -> Maybe Blame
findObservationWithProv obs provs = Map.lookup obs provs

testElementProv :: FnSym -> [Element] -> ElementProvs -> Maybe Element
testElementProv skFn elms provs = do
  skTerm <- if null elms
              then return $ Cons $ Constant skFn
              else do
                terms <- mapM ((flip getElementProv) provs) elms
                return $ Fn skFn terms
  findElementWithProv skTerm provs