{-|
  This module defines an instance of a Chase Model serialized into an XML String
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
data TableRef  = ConTable Sym -- constant tables
               | RelTable Sym -- relation tables
               | FunTable Sym -- function tables
               | DomTable -- A table containing elements of the domain
               | TmpTable -- A temporary table denoting equality between 
                          -- elements (C-rules in Bachmair et al. definitions)
-}
xpTableRef :: PU TableRef
xpTableRef =
  xpWrap  ( \ ((reftype, refname)) -> (typeTable reftype refname)
    , \ t -> (tableType t, 
              tableName t)
    ) $
  xpPair  (xpAttr "type" xpText)
          (xpOption (xpAttr "name" xpText))

xpTable :: PU Table
xpTable = xpElem "table" (xpZero "Not implemented")

xpProvInfo :: PU ProvInfo
xpProvInfo = xpElem "provinfo" (xpZero "Not implemented")

xpElemHistory :: PU ElemHistory
xpElemHistory = xpElem "elemhist" (xpZero "Not implemented")