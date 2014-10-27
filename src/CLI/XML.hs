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
import Syntax.GeometricUtils
import Syntax.Geometric
import SAT.Impl (SATIteratorType)
import SAT.Data
import Data.Maybe 
import Data.List

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
-}
xpProvInfo :: PU ProvInfo
xpProvInfo = 
  xpElem "PROVINFO" $
  xpWrap (\xml@(prov) -> emptyProvInfo
    , \prov -> "not implemented"
    ) $ 
  xpText

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
