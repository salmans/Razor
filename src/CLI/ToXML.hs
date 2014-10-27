{-|
  Razor
  Module      : CLI.ToXML
  Description : The module provides a XML pickling for the state of command line interface
  Maintainer  : Salman Saghafi, Ryan Danas
-}

module CLI.ToXML where
import Text.XML.HXT.Arrow.Pickle
import Text.XML.HXT.Core
import API.Surface
import API.Core
import Common.IModel
import Common.Provenance
import Common.IObservation
import Syntax.Term
import Syntax.GeometricUtils
import Syntax.Geometric
import SAT.Impl (SATIteratorType)
import SAT.Data
import Data.Maybe 
import Data.List
import qualified Data.Map as Map 

-----------------
-- XML HELPERS --
-----------------
xmlConfigOUT = [withIndent yes]

toXMLString :: (XmlPickler a) => a -> String
toXMLString struct = showPickled xmlConfigOUT struct

toXMLFile :: (XmlPickler a) => a -> String -> IO ()
toXMLFile struct file = do 
  let str = toXMLString struct
  writeFile file str

------------
-- USTATE --
------------
instance XmlPickler UState where xpickle = xpState
{-
  type UState = (Theory, ProvInfo, SATIteratorType, Model, ModelProv)
-}
xpState :: PU UState
xpState =
	xpElem "STATE" $
	xpWrap (\xml@((thy, stream, mdl, prov, mdlprov)) -> (UState thy prov stream mdl mdlprov)
		, \ustate@(UState thy prov stream mdl mdlprov) -> (thy, stream, mdl, prov, mdlprov)
		) $
	xp5Tuple xpTheory xpStream xpModel xpProvInfo xpModelProv

------------
-- THEORY --
------------
{-
  type Theory = [Sequent]
-}
xpTheory :: PU Theory
xpTheory = 
	xpElem "THEORY" $
  xpWrap (\rules -> (map (\(i, r)->r) rules)
    ,\sequents -> (map (\s->( (fromMaybe 0 (elemIndex s sequents)), s) ) sequents)
  ) $ 
  xpList xpRule
xpRule :: PU (Int, Sequent)
xpRule = xpElem "RULE" $ xpPair (xpAttr "ID" xpPrim) xpSequent
{-
  data Sequent = Sequent {
      sequentBody :: Formula,
      sequentHead :: Formula
    }
-}
xpSequent :: PU Sequent
xpSequent = xpWrap (parseSequent, show) xpText

------------
-- STREAM --
------------
-- TODO not implemented yet; needs to change structure on the haskell side first
xpStream :: PU SATIteratorType
xpStream = xpWrap (\()->(satInitialize emptySATTheory), \anything->()) xpUnit

-----------
-- MODEL --
-----------
{-
data Model = Model { modelElements     :: Map.Map Element [Element]
                   , modelObservations :: [Observation] }
-}
xpModel :: PU Model
xpModel = 
  xpElem "MODEL" $
  xpWrap (\xml@(elms, obvs) -> (Model elms obvs)
   , \(Model elms obvs) -> (elms, obvs)
   ) $ 
  xpPair xpModelElements xpModelObservations
xpModelElements :: PU (Map.Map Element [Element])
xpModelElements =
  xpElem "MODELELEMENTS" $
  xpWrap (Map.fromList, Map.toList) $
  xpList $
  xpElem "MDLELM" $ 
  xpPair (xpElem "ACTUAL" xpElement) (xpElem "EQUAL" (xpList xpElement))
xpModelObservations :: PU [Observation]
xpModelObservations =
  xpElem "MODELOBSERVATIONS" $
  xpList (xpElem "MDLOBS" xpObservation)

--------------
-- PROVINFO --
--------------
{-
data ProvInfo = ProvInfo { elementProvs     :: ElementProvs
                         , observationProvs :: ObservationProvs
                         }
-}
xpProvInfo :: PU ProvInfo
xpProvInfo = 
  xpElem "PROVINFO" $
  xpWrap (\xml@(elmProv, obvProv) -> (ProvInfo elmProv obvProv)
   , \prov@(ProvInfo elmProv obvProv) -> (elmProv, obvProv)
   ) $ 
  xpPair xpElementProvs xpObservationProvs
{-
type ElementProvs = ( Map.Map Element [SkolemTerm]
                    , Map.Map SkolemTerm Element)
-}
xpElementProvs :: PU (Map.Map Element [Term], Map.Map Term Element)
xpElementProvs =
  xpElem "ELEMENTPROVS" $
  xpPair xpElmToTerms xpTermsToElm
xpElmToTerms :: PU (Map.Map Element [Term])
xpElmToTerms =
  xpWrap (Map.fromList, Map.toList) $
  xpList $
  xpElem "ELMPROV" $ 
  xpPair (xpElem "FROM" xpElement) (xpElem "TO" (xpList xpTerms))
xpTermsToElm :: PU (Map.Map Term Element)
xpTermsToElm =
  xpWrap (Map.fromList, Map.toList) $
  xpList $
  xpElem "ELMPROV" $ 
  xpPair (xpElem "FROM" xpTerms) (xpElem "TO" xpElement)
{-
type ObservationProvs = Map.Map Observation Blame 
-}
xpObservationProvs :: PU (Map.Map Observation Blame)
xpObservationProvs =
  xpElem "OBSERVATIONPROVS" $
  xpWrap (Map.fromList, Map.toList) $
  xpList $
  xpElem "OBSPROV" $ 
  xpPair (xpElem "FROM" xpObservation) (xpElem "TO" xpBlame)
{-
data Blame = TheoryBlame Id Sub
-}
xpBlame :: PU Blame
xpBlame =
  xpElem "BLAME" $ 
  xpWrap (\xml@(i, s) -> (TheoryBlame i s)
   , \blame@(TheoryBlame i s) -> (i, s)
   ) $ 
  xpPair (xpAttr "RULEID" xpPrim) xpSub
{-
type FreeSub = Sub
type Sub = Map.Map Variable Term
-}
xpSub :: PU FreeSub
xpSub = 
  xpWrap (Map.fromList, Map.toList) $
  xpList $
  xpElem "FREESUB" $ 
  xpPair (xpElem "FROM" xpVariable) (xpElem "TO" xpTerms)

---------------
-- MODELPROV --
---------------
-- TODO should this be sent as XML via the CLI?
-- its quite a large struct
-- better to just query the CLI for the particular model prov info desired?
xpModelProv :: PU ModelProv
xpModelProv = xpWrap (\()->emptyModelProv, \anything->()) xpUnit

------------------
-- TERM RELATED --
------------------
xpTerms :: PU Term
xpTerms = 
  xpElem "TERM" $
  xpWrap (parseTerm, show) xpText

xpElement :: PU Element
xpElement = 
  xpWrap (\term->(fromMaybe (Element "") (termToElement term)), (\e->(Elem e))) xpTerms

xpVariable :: PU Variable
xpVariable = xpWrap ((\term->(fromMaybe (Variable "") (termToVariable term))), (\v->(Var v))) xpTerms

---------------------
-- FORMULA RELATED --
---------------------
xpFormula :: PU Formula
xpFormula = xpWrap (parseFormula, show) xpText

xpAtoms :: PU Atom
xpAtoms = xpWrap ((\fml->(fromMaybe (Rel "" []) (formulaToAtom fml))), \atm->(Atm atm)) xpFormula
formulaToAtom :: Formula -> Maybe Atom
formulaToAtom (Atm a) = Just a
formulaToAtom _ = Nothing 

xpObservation :: PU Observation
xpObservation = xpWrap ((\atom->(fromMaybe (Obs (Rel "" [])) (toObservation atom))), (\(Obs a)->a)) xpAtoms
