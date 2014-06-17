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

xpElemHistory :: PU ElemHistory
xpElemHistory = xpElem "elemhist" (xpZero "Not implemented")