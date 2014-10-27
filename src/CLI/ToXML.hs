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
import Control.DeepSeq
import qualified Data.Map as Map 

-----------------
-- XML HELPERS --
-----------------
xmlConfigOUT = [withIndent yes]
xmlConfigIN = [withValidate no, withRemoveWS yes, withPreserveComment no]

toXMLString :: (XmlPickler a) => a -> String
toXMLString struct = showPickled xmlConfigOUT struct

toXMLFile :: UState -> String -> IO ()
toXMLFile state@(UState theory prov stream model modelProv) file = do 
  let str = (toXMLString (XMLState theory prov model))
  deepseq str (writeFile file str)

fromXMLFile :: String -> IO (UState)
fromXMLFile file = do
  states <- runX ((xunpickleDocument xpState xmlConfigIN file)>>>processState)
  let (XMLState theory prov model) = head states 
  return (UState theory prov (satInitialize emptySATTheory) model emptyModelProv)

processState :: IOSArrow XMLState XMLState
processState = arrIO (\x -> do{return x})
------------
-- STATE --
------------
data XMLState = XMLState Theory ProvInfo Model
instance XmlPickler XMLState where xpickle = xpState
xpState :: PU XMLState
xpState =
	xpElem "STATE" $
	xpWrap (\((thy, mdl, prov)) -> (XMLState thy prov mdl)
		, \ustate@(XMLState thy prov mdl) -> (thy, mdl, prov)
		) $
	xpTriple xpTheory xpModel xpProvInfo

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
    ,\sequents -> (map (\s->( (fromMaybe (error "not in list") (elemIndex s sequents)), s) ) sequents)
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
xpSequent = 
  xpWrap (\(bd, hd)->(Sequent bd hd), \(Sequent bd hd)->(bd,hd)) $
  xpPair (xpElem "BODY" xpFormula) (xpElem "HEAD" xpFormula)


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
  xpWrap (\(elms, obvs) -> (Model elms obvs)
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
  xpWrap (\(elmProv, obvProv) -> (ProvInfo elmProv obvProv)
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
  xpWrap (\(i, s) -> (TheoryBlame i s)
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
-- TODO better way to parse terms?
xpTerms :: PU Term
xpTerms =
  xpElem "TERM" $
  xpWrap (implodeTerm, explodeTerm) $
  xpTriple (xpAttr "TYPE" xpText) (xpAttr "VALUE" xpText) (xpList xpTerms)

implodeTerm :: (String, String, [Term]) -> Term
implodeTerm ("VARIABLE", v, []) = (Var (Variable v))
implodeTerm ("CONSTANT", c, []) = (Cons (Constant c))
implodeTerm ("ELEMENT", e, []) = (Elem (Element e))
implodeTerm ("FUNCTION", s, terms) = (Fn s terms)
implodeTerm _ = error "unknown term type"

explodeTerm :: Term -> (String, String, [Term])
explodeTerm (Var (Variable v)) = ("VARIABLE", v, [])
explodeTerm (Cons (Constant c)) = ("CONSTANT", c, [])
explodeTerm (Elem (Element e)) = ("ELEMENT", e, [])
explodeTerm (Fn s terms) = ("FUNCTION", s, terms)
explodeTerm _ = ("UNKNOWN", "", [])

xpElement :: PU Element
xpElement = xpWrap (\term->(fromMaybe (error ((show term)++" is not an element")) (termToElement term)), \elm->(Elem elm)) xpTerms

xpVariable :: PU Variable
xpVariable = xpWrap (\term->(fromMaybe (error ((show term)++" is not a variable")) (termToVariable term)), \elm->(Var elm)) xpTerms

---------------------
-- FORMULA RELATED --
---------------------
xpFormula :: PU Formula
xpFormula = 
  xpWrap (parseFormula, show) $
  xpText

xpXFormula :: PU Formula
xpXFormula = 
  xpWrap (xparseFormula, show) $
  xpText

xpAtoms :: PU Atom
xpAtoms = xpWrap (\fml->(fromMaybe (error ((show fml)++" is not an atom")) (formulaToAtom fml)), \atm->(Atm atm)) xpXFormula
formulaToAtom :: Formula -> Maybe Atom
formulaToAtom (Atm a) = Just a
formulaToAtom _ = Nothing

xpObservation :: PU Observation
xpObservation = xpWrap ((\atom->(fromMaybe (error ((show atom)++" is not an observation")) (toObservation atom))), (\(Obs a)->a)) xpAtoms
