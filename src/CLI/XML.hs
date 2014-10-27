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
import SAT.Impl (SATIteratorType)
import SAT.Data

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
	xpElem "USTATE" $
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
  xpWrap (\xml@(sequents) -> sequents
  , \theory@(sequents) -> sequents
  ) $ 
  xpList xpSequent
{-
  data Sequent = Sequent {
      sequentBody :: Formula,
      sequentHead :: Formula
    }
-}
xpSequent :: PU Sequent
xpSequent =
  xpElem "RULE" $
  xpWrap (\xml@(bd, hd) -> (Sequent bd hd)
    , \sequent@(Sequent bd hd) -> (bd, hd)
    ) $
  xpPair (xpElem "BODY" xpFormula) (xpElem "HEAD" xpFormula)
{-
data Formula = Tru
             | Fls
             | Atm Atom
             | And Formula Formula
             | Or  Formula Formula
             | Exists (Maybe FnSym) Variable Formula
             | Lone (Maybe FnSym) Variable Formula Formula
-}
xpFormula :: PU Formula
xpFormula =
  xpWrap (\xml@(fml) -> Tru
    , \fml -> "not implemented"
    ) $
  xpText

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
{-
-}
xpStream :: PU SATIteratorType
xpStream = 
  xpElem "STREAM" $
  xpWrap (\xml@(stream) -> satInitialize emptySATTheory
    , \stream -> "not implemented"
    ) $
  xpText

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
{-
-}
xpModelProv :: PU ModelProv
xpModelProv = 
  xpElem "MODELPROV" $
  xpWrap (\xml@(mdlprov) -> emptyModelProv
    , \mdlprov -> "not implemented"
    ) $
  xpText
