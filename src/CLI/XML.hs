{-|
  Razor
  Module      : CLI.XML
  Description : The module provides a XML conversion for the state of command line interface
  Maintainer  : Salman Saghafi, Ryan Danas
-}

module CLI.XML where
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

toXMLFile :: UState -> Maybe UAnswer -> String -> IO ()
toXMLFile state@(UState theory prov stream model modelProv) answer file = do
  let str = toXMLString (XMLRoot answer (XMLState theory prov model))
  deepseq str (writeFile file str)

toXMLString :: (XmlPickler a) => a -> String
toXMLString struct = showPickled xmlConfigOUT struct

fromXMLFile :: String -> IO (UState)
fromXMLFile file = do
  roots <- runX ((xunpickleDocument xpRoot xmlConfigIN file)>>>processXML)
  let (XMLRoot answer (XMLState theory prov model)) = head roots 
  return (UState theory prov (satInitialize emptySATTheory) model emptyModelProv)

processXML :: IOSArrow XMLRoot XMLRoot
processXML = arrIO (\x -> do{return x})

----------
-- ROOT --
----------
data XMLRoot = XMLRoot (Maybe UAnswer) XMLState
instance XmlPickler XMLRoot where xpickle = xpRoot
xpRoot :: PU XMLRoot
xpRoot =
  xpElem "RAZOR" $
  xpWrap (\(ans, state)->(XMLRoot ans state), \(XMLRoot ans state)->(ans, state)) $
  xpPair xpAnswer xpState

------------
-- ANSWER --
------------
{-
data UAnswer = AOrigin UOrigin | ABlame UBlame
-}
xpAnswer :: PU (Maybe UAnswer)
xpAnswer = 
  xpElem "ANSWER" $
  xpWrap (implodeAnswer, explodeAnswer) $
  xpPair (xpOption xpOrigin) (xpOption xpBlame)

implodeAnswer :: (Maybe UOrigin, Maybe UBlame) -> Maybe UAnswer
implodeAnswer _ = Nothing

explodeAnswer :: Maybe UAnswer -> (Maybe UOrigin, Maybe UBlame)
explodeAnswer Nothing = (Nothing, Nothing)
explodeAnswer (Just(AOrigin origin)) = (Just origin, Nothing)
explodeAnswer (Just(ABlame blame)) = (Nothing, Just blame)

------------
-- ORIGIN --
------------
{-
data UOrigin = UOriginLeaf Term (Either UError TheorySub) | UOriginNode Term (Either UError TheorySub) [UOrigin]
-}
xpOrigin :: PU UOrigin
xpOrigin =
  xpElem "ORIGIN" $
  xpWrap (\()->(error "no"), \anything->()) xpUnit

-----------
-- BLAME --
-----------
{-
type UBlame = Either UError TheorySub
-}
xpBlame :: PU UBlame
xpBlame =
  xpElem "BLAME" $
  xpWrap (\()->(error "no"), \anything->()) xpUnit

-----------
-- STATE --
-----------
data XMLState = XMLState Theory ProvInfo Model
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
xpSequent = xpWrap (parseSequent, show) $ xpText


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
  xpElem "ETS" $ 
  xpPair (xpElem "FROM" xpElement) (xpElem "TO" (xpList xpTerms))
xpTermsToElm :: PU (Map.Map Term Element)
xpTermsToElm =
  xpWrap (Map.fromList, Map.toList) $
  xpList $
  xpElem "STE" $ 
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
  xpPair (xpElem "FROM" xpObservation) (xpElem "TO" xpThyBlame)
{-
data Blame = TheoryBlame Id Sub
-}
xpThyBlame :: PU Blame
xpThyBlame =
  xpElem "THYBLAME" $ 
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

------------------
-- TERM RELATED --
------------------
{-
data Term = Var  Variable
          | Cons Constant
          | Elem Element
          | Fn FnSym [Term]
-}
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

------------------
-- ATOM RELATED --
------------------
{-
data Atom = Rel   RelSym [Term]
          | FnRel FnSym  [Term]
-}
xpAtoms :: PU Atom
xpAtoms =
  xpElem "ATOM" $
  xpWrap (implodeAtom, explodeAtom) $
  xpTriple (xpAttr "TYPE" xpText) (xpAttr "NAME" xpText) (xpList xpTerms)

explodeAtom :: Atom -> (String, String, [Term])
explodeAtom (Rel s terms) = ("RELATION", s, terms)
explodeAtom (FnRel s terms) = ("FUNCTION", s, terms)

implodeAtom :: (String, String, [Term]) -> Atom
implodeAtom ("RELATION", s, terms) = (Rel s terms)
implodeAtom ("FUNCTION", s, terms) = (FnRel s terms) 

xpObservation :: PU Observation
xpObservation = xpWrap (\atom->(Obs atom), \(Obs a)->a) xpAtoms
