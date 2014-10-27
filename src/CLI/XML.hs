{-|
  Razor
  Module      : CLI.XML
  Description : The module provides a XML pickling / depicking for the I/O data structures of the command line interface. 
  Maintainer  : Salman Saghafi, Ryan Danas
-}

module CLI.XML where
import Text.XML.HXT.Arrow.Pickle
import Text.XML.HXT.Core
import API.Surface
import API.Core
import Common.Model
import Common.Provenance
import Common.Observation
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
	xpWrap (\xml@((thy, prov, stream, mdl, mdlprov)) -> (UState thy prov stream mdl mdlprov)
		, \ustate@(UState thy prov stream mdl mdlprov) -> (thy, prov, stream, mdl, mdlprov)
		) $
	xp5Tuple xpTheory xpProvInfo xpStream xpModel xpModelProv

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
xpObservationProvs = xpWrap (\()->Map.empty, \anything->()) xpUnit
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
-}
xpModel :: PU Model
xpModel = 
  xpElem "MODEL" $
  xpWrap (\xml@(mdl) -> emptyModel
    , \mdl -> "not implemented"
    ) $
  xpText

---------------
-- MODELPROV --
---------------
-- TODO should this be sent as XML via the CLI?
-- its quite a large struct
-- better to just query the CLI for the particular model prov info desired?
xpModelProv :: PU ModelProv
xpModelProv = xpWrap (\()->emptyModelProv, \anything->()) xpUnit

-------------
-- GENERAL --
-------------
xpElement :: PU Element
xpElement = 
  xpElem "ELEMENT" $ 
  xpWrap (\xml->(fromMaybe (Element "") (termToElement (parseTerm xml))), (\e->(show (Elem e)))) xpText

xpTerms :: PU Term
xpTerms = 
  xpElem "TERM" $
  xpWrap (parseTerm, show) xpText