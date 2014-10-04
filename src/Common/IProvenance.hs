{-| 
  Razor
  Module      : Common.IProvenance
  Description : The module defines provenance information for elements and facts
  of a model and provides functions to read and modify it.
  Maintainer  : Salman Saghafi -}
module Common.IProvenance where

-- Standard
import qualified Data.Map as Map
import Data.Maybe

-- Control
import Control.Applicative

-- Syntax
import Syntax.Term (Term (..), Element (..), Constant (..), Sub, FnSym)

-- Common
import Common.Basic (Id)
import Common.Observation (Observation)

unitName               = "Common.Provenance"
error_missingProv      = "provenance information for element is missing"

{-| Provenance information for elements are stored as skolem terms. -}
type SkolemTerm = Term

{-| Provenance of elements that are created by existential quantifiers are 
  stored as an ElemHistory structure inside a model. 
  Because we need to search in 'ElementProvs' in both directions, we maintain
  a pair of maps from elements to the provenance terms and from the provenance 
  terms to elements. -}
type ElementProvs = ( Map.Map Element [SkolemTerm]
                    , Map.Map SkolemTerm Element)

{-| Blaming information for an 'Observation' is a sequent of the theory (or the
  extended theory after augmentation), which is identified by its 'Id', together
  with a binding from its free variables to elements of the model. -}
data Blame = TheoryBlame Id Sub
           deriving (Show, Eq, Ord)

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
emptyProvInfoWithConstants consts = 
    let elms = (\(Constant c) -> Element c) <$> consts
        cs   = Cons <$> consts
    in  ProvInfo ( Map.fromList $ zip elms (pure <$> cs)
                 , Map.fromList $ zip cs elms )
                 Map.empty


{-| Returns the skolem term, i.e., the provenance information, for the input
  element. -}
getElementProv :: Element -> ElementProvs -> [SkolemTerm]
getElementProv elm (provs, _) = Map.findWithDefault [] elm provs

{-| Adds provenance information for the input 'Element' where the input skolem 
  function has been assigned to the existential quantifier that creates the
  element and the input list of elements are the parameters of the Skolem 
  function. -}
addElementProv :: Element -> FnSym -> [Element] -> ElementProvs -> 
                  ElementProvs
addElementProv elm fn elms provs = 
    let terms  = (flip getElementProv) provs <$> elms 
        terms' = combinations terms
    in  if   any null terms
        then error $ unitName ++ ".addElementProv: " ++ error_missingProv
        else insertProv elm (if   null elms 
                             then [Cons (Constant fn)]
                             else (Fn fn) <$> terms') provs
    where combinations [l]     = [l]
          combinations (l:ls)  = [x:xs | x <- l, xs <- combinations ls]


insertProv :: Element -> [SkolemTerm] -> ElementProvs -> ElementProvs
insertProv elm terms (elemProvs, provElems) =
    let provElems' = foldr (\t ps -> Map.insert t elm ps) provElems terms
    in  (Map.insert elm terms elemProvs, provElems')


{-| Adds provenance information for the input 'Element' where the input skolem 
  function has been assigned to the existential quantifier that creates the
  element and the input list of elements are the parameters of the Skolem 
  function. -}
findElementWithProv :: SkolemTerm -> ElementProvs -> Maybe Element
findElementWithProv term (_, provs) =  Map.lookup term provs