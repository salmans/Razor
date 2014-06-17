{-
  This module defines an instance of a Chase Model serialized into/outof an XML String
  NOTE: Going from XML to Haskell Data Structures has not been tested
-}
module Chase.Problem.XMLModel where

-- General Modules
import Data.Map (fromList, toList)
import Data.Maybe
-- Logic Modules
import Formula.SyntaxGeo
import Utils.GeoUtilities
-- Chase Modules
import Chase.Problem.Model
import Chase.Problem.RelAlg.IRelAlg
import Chase.Problem.BaseTypes
import qualified RelAlg.DB as DB
-- HXT Library Modules
import Text.XML.HXT.Arrow.Pickle
import Text.XML.HXT.Core

{-
  data Model = Model
  {
        modelTables   :: Tables,
        modelProvInfo :: ProvInfo,
        modelElemHist :: ElemHistory
  }
-}
instance XmlPickler Model where xpickle = xpModel
xpModel :: PU Model
xpModel =
  xpElem "model" $
  xpWrap ( \ ((tbls, provinfo, elemhist)) -> Model tbls provinfo elemhist
    , \ t -> (modelTables t,
              modelProvInfo t,
              modelElemHist t)
    ) $
  xpTriple  xpTables
            xpProvInfo
            xpElemHistory

{-
type Tables = Map TableRef Table
-}
xpTables :: PU Tables
xpTables = 
  xpWrap ( fromList, toList ) $
  xpList $
  xpElem "table" $
  xpPair  xpTableRef
          xpTable

{-
data TableRef  = ConTable Sym -- constant tables
               | RelTable Sym -- relation tables
               | FunTable Sym -- function tables
               | DomTable -- A table containing elements of the domain
               | TmpTable -- A temporary table denoting equality between 
                          -- elements (C-rules in Bachmair et al. definitions)
type Sym = String
-}
xpTableRef :: PU TableRef
xpTableRef =
  xpWrap  ( \ ((reftype, refname)) -> (typeTable reftype refname)
    , \ t -> (tableType t, 
              tableName t)
    ) $
  xpPair  (xpAttr "type" xpText)
          (xpOption (xpAttr "name" xpText))
-- TODO: I think there's a way to avoid using the two following functions for disjunct types
-- Maps a table ref type to a string
tableType :: TableRef -> String
tableType (ConTable s)  = "Constant"
tableType (RelTable s)  = "Relation"
tableType (FunTable s)  = "Function"
tableType (DomTable)    = "Domain"
tableType (TmpTable)    = "Temporary"
-- Maps a string to a table ref type
typeTable :: String -> Maybe Sym -> TableRef
typeTable "Constant" (Just s)  = (ConTable s)
typeTable "Relation" (Just s)  = (RelTable s)
typeTable "Function" (Just s)  = (FunTable s)
typeTable "Domain"    Nothing  = (DomTable)
typeTable "Temporary" Nothing  = (TmpTable)

{-
type Table = DB.Set Record
data Set a = Set [a] 
... substitution ...
type Table = Set [Record]
... calling records ...
type Table = [Record]
-}
xpTable :: PU Table
xpTable =
  xpWrap ( \ ((rs)) -> DB.Set rs
    , \ t -> (records t)
    ) $
  (xpList xpRecord)

{-
type Record = [Elem]
-}
xpRecord :: PU Record
xpRecord = (xpElem "record" (xpList xpElt))

{-
newtype Elem = Elem Sym
type Sym = String
-}
xpElt :: PU Elem
xpElt =
  xpWrap ( \ ((e)) -> Elem e
      , \ (Elem e) -> (e)
      ) $
  (xpAttr "elt" xpText)

xpProvInfo :: PU ProvInfo
xpProvInfo = xpElem "provinfo" (xpZero "Not implemented")

{-
type ElemHistory = [(Elem, SkolemTerm)]
-}
xpElemHistory :: PU ElemHistory
xpElemHistory = (xpList xpElemProvInfo)

xpElemProvInfo :: PU (Elem, SkolemTerm)
xpElemProvInfo =
  xpElem "history" $
  xpWrap ( \ ((e, st)) -> (e, st)
    , \ t -> (fst t, snd t)
    ) $
  xpPair xpElt xpSkolemTerm

{-
type SkolemTerm = Term
data Term = Var Var
          | Elm Elem -- elements of the domain are terms
          | Fn Sym [Term]
          | Rn Sym [Term] -- Just like Fn but for cases where we treat relations
                          -- like functions.
-}
xpSkolemTerm :: PU SkolemTerm
xpSkolemTerm = 
  xpElem "SkolemTerm" $
  xpWrap ( \ ((termtype, value, moreterms)) -> implodeSkolemTerm (termtype, value, moreterms)
    , \ t -> (explodeSkolemTerm t)
    ) $
  xpTriple  (xpAttr "type" xpText)
            (xpAttr "value" xpText)
            (xpList xpSkolemTerm)
-- TODO: again, better way to do this?
-- Maps a skolem term to a xml-friendly value tuple
explodeSkolemTerm :: SkolemTerm -> (String, String, [Term])
explodeSkolemTerm (Var v)         = ("Variable", v, [])
explodeSkolemTerm (Elm (Elem e))  = ("Element", e, [])
explodeSkolemTerm (Fn s terms)    = ("Function", s, terms)
explodeSkolemTerm (Rn s terms)    = ("Relation", s, terms)
-- Unmaps a skolem term to a xml-friendly value tuple
implodeSkolemTerm :: (String, String, [Term]) -> SkolemTerm
implodeSkolemTerm ("Variable", v, [])  = (Var v)
implodeSkolemTerm ("Element", e, [])   = (Elm (Elem e))
implodeSkolemTerm ("Function", s, terms)    = (Fn s terms)
implodeSkolemTerm ("Relation", s, terms)    = (Rn s terms)