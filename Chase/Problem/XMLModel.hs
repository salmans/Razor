{-
  This module defines an instance of a Chase Model serialized into/outof an XML String
  NOTE: Going from XML to Haskell Data Structures has not been tested
-}
module Chase.Problem.XMLModel where

-- General Modules
import Data.Map (fromList, toList, Map)
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

instance XmlPickler Model where xpickle = xpModel
{-
  data Model = Model
  {
        modelTables   :: Tables,
        modelProvInfo :: ProvInfo,
        modelElemHist :: ElemHistory
  }
-}
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
            xpElemHistories

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
  xpWrap  ( \ ((reftype, refname)) -> (implodeTableRef (reftype, refname))
    , \ t -> (explodeTableRef t)
    ) $
  xpPair  (xpAttr "type" xpText)
          (xpOption (xpAttr "name" xpText))
{- 
TODO: I think there's a way to avoid using the two following functions for disjunct types
      I tried avoiding doing this for data types with clauses (disjunct types in Coq); didn't work
      The problem lies within how the clause is treated. The clause isn't really its own type or data constructor, 
      so I don't think there is a way to make its own pickler. 
      This means the clause "type" has to be an attribute instead of it's own element.
      Good or bad? I don't know at this point.
-}
-- Maps a TableRef to an XML friendly tuple
explodeTableRef :: TableRef -> (String, Maybe Sym)
explodeTableRef (ConTable s)  = ("Constant", Just s)
explodeTableRef (RelTable s)  = ("Relation", Just s)
explodeTableRef (FunTable s)  = ("Function", Just s)
explodeTableRef (DomTable)    = ("Domain", Nothing)
explodeTableRef (TmpTable)    = ("Temporary", Nothing)
-- Maps an XML tuple to a TableRef
implodeTableRef :: (String , Maybe Sym) -> TableRef
implodeTableRef ("Constant", (Just s))  = (ConTable s)
implodeTableRef ("Relation", (Just s))  = (RelTable s)
implodeTableRef ("Function", (Just s))  = (FunTable s)
implodeTableRef ("Domain", Nothing)     = (DomTable)
implodeTableRef ("Temporary", Nothing)  = (TmpTable)

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

{-
data ProvInfo = ProvInfo { provInfoData     :: Map.Map Obs [Prov]
                         , provInfoLastTag  :: ProvTag }
type ProvTag = Int
-}
xpProvInfo :: PU ProvInfo
xpProvInfo =
  xpElem "provenanceinformation" $
  xpWrap ( \ ((pidata, pilasttag)) -> ProvInfo pidata pilasttag
    , \ t -> (provInfoData t,
              provInfoLastTag t)
    ) $
  xpPair  xpProvInfoData
          (xpAttr "total" xpPrim)

{-
provInfoData     :: Map.Map Obs [Prov]
-}
xpProvInfoData :: PU (Map Obs [Prov])
xpProvInfoData = 
  xpWrap ( fromList, toList ) $
  xpList $
  xpElem "provinfo" $
  xpPair  xpObs
          (xpList xpProv)

{-
data Obs = Eql Term Term  -- Two terms are equal
         | Fct Atom       -- Atm is true as a fact
-}
xpObs :: PU Obs
xpObs = 
  xpElem "obs" $
  xpWrap ( \ ((obstype, t1, t2, atom)) -> implodeObs (obstype, t1, t2, atom)
      , \ t -> (explodeObs t)
  ) $
  xp4Tuple  (xpAttr "type" xpText)
            (xpOption xpSkolemTerm)
            (xpOption xpSkolemTerm)
            (xpOption xpAtom)
-- Maps an observation to a xml-friendly value tuple
explodeObs :: Obs -> (String, Maybe Term, Maybe Term, Maybe Atom)
explodeObs (Eql t1 t2)         = ("Equality", Just t1, Just t2, Nothing)
explodeObs (Fct a)             = ("Fact", Nothing, Nothing, Just a)
-- Unmaps a observation to a xml-friendly value tuple
implodeObs :: (String, Maybe Term, Maybe Term, Maybe Atom) -> Obs
implodeObs ("Equality", Just t1, Just t2, Nothing)  = (Eql t1 t2)
implodeObs ("Fact", Nothing, Nothing, Just a)       = (Fct a)

{-
data Atom = R Sym [Term]
          | F Sym [Term]
-}
xpAtom :: PU Atom
xpAtom =
  xpElem "atom" $
  xpWrap ( \ ((atomtype, name, terms)) -> implodeAtom (atomtype, name, terms)
      , \ t -> (explodeAtom t)
  ) $
  xpTriple  (xpAttr "type" xpText)
            (xpAttr "name" xpText)
            (xpList xpSkolemTerm)
-- Maps an atom to a xml-friendly value tuple
explodeAtom :: Atom -> (String, String, [Term])
explodeAtom (R s terms)              = ("Relation", s, terms)
explodeAtom (F s terms)              = ("Function", s, terms)
-- Unmaps a atom from a xml-friendly value tuple
implodeAtom :: (String, String, [Term]) -> Atom
implodeAtom ("Relation", s, terms) = (R s terms) 
implodeAtom ("Function", s, terms) = (F s terms) 

{-
data Prov = ChaseProv ProvTag ID Sub
          | UserProv
type ProvTag = Int
type ID = Int
-}
xpProv :: PU Prov
xpProv =
  xpElem "prov" $
  xpWrap ( \ ((provtype, tag, i, sub)) -> implodeProv (provtype, tag, i, sub)
      , \ t -> (explodeProv t)
  ) $
  xp4Tuple  (xpAttr "type" xpText)
            (xpOption (xpAttr "tag" xpPrim))
            (xpOption (xpAttr "id" xpPrim))
            (xpOption xpSub)
-- Maps a provenance to a xml-friendly value tuple
explodeProv :: Prov -> (String, Maybe Int, Maybe Int, Maybe Sub)
explodeProv (ChaseProv tag i sub)             = ("Chase", Just tag, Just i, Just sub)
explodeProv (UserProv)              = ("User", Nothing, Nothing, Nothing)
-- Unmaps a provenance from a xml-friendly value tuple
implodeProv :: (String, Maybe Int, Maybe Int, Maybe Sub) -> Prov
implodeProv ("Chase", Just tag, Just i, Just sub) = (ChaseProv tag i sub) 
implodeProv ("User", Nothing, Nothing, Nothing) = (UserProv)

{-
type Sub = Map Var Term
type Var = String
-}
xpSub :: PU Sub
xpSub =
  xpWrap ( fromList, toList ) $
  xpList $
  xpElem "sub" $
  xpPair  (xpAttr "var" xpText)
          (xpSkolemTerm)

{-
type ElemHistory = [(Elem, SkolemTerm)]
-}
xpElemHistories :: PU ElemHistory
xpElemHistories = (xpList xpElemHistory)

xpElemHistory :: PU (Elem, SkolemTerm)
xpElemHistory =
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
  xpElem "term" $
  xpWrap ( \ ((termtype, value, moreterms)) -> implodeSkolemTerm (termtype, value, moreterms)
    , \ t -> (explodeSkolemTerm t)
    ) $
  xpTriple  (xpAttr "type" xpText)
            (xpAttr "value" xpText)
            (xpList xpSkolemTerm)
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