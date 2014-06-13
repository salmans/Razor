{-|
  This module defines an instance of a Chase Model serialized into an XML String
-}
module Chase.Problem.XMLModel where

-- General Modules

-- Logic Modules

-- Chase Modules
import Chase.Problem.Model
import Chase.Problem.RelAlg.RelAlg
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

--instance XmlPickler Tables where xpickle = xpTables
xpTables :: PU Tables
xpTables = xpElem "tables" (xpZero "Tables not implemented")

--instance XmlPickler ProvInfo where xpickle = xpProvInfo
xpProvInfo :: PU ProvInfo
xpProvInfo = xpElem "provinfo" (xpZero "ProvInfo not implemented")

--instance XmlPickler ElemHistory where xpickle = xpElemHistory
xpElemHistory :: PU ElemHistory
xpElemHistory = xpElem "elemhist" (xpZero "ElemHistory not implemented")