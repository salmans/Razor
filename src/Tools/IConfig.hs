{- Razor
   Module      : Tools.IConfig
   Description : Implements a datatype 'Config' and its corresponding functions
   for managing user preferences.
   Maintainer  : Salman Saghafi -}

module Tools.IConfig where

-- Standard
import Data.Maybe

-- Control
import Control.Applicative
import Control.Monad.State.Lazy as State

-- Common
import Common.Data (SkolemDepthMap, emptySkolemDepthMap)

{-| The type of input formula: 
 [@GeoLog@] Razor's input geometric form 
 [@TPTPCNF@] CNF formulas in TPTP format 
 [@TPTPFOF@] First-order formulas (FOF) in TPTP format -}
data InputType = GeoLog | TPTPCNF | TPTPFOF

instance Show InputType where
    show GeoLog  = "geo"
    show TPTPCNF = "cnf"
    show TPTPFOF = "fof"

{-| Config is a datatype to store user preferences and pass them around. 
 [@configInput@] a string containing the address of the input theory file
 [@configDebug@] turns debug on/off
 [@configIncremental@] enable/disable incremental view maintenance 
 [@configAllModels@] return all models 
 [@configBound@] optionally puts a bound on the size of the models
 [@configTPTPPath@] the address to TPTP root directory for loading standard 
 axioms and formulas
 [@configInputType@] an instance of 'InputType' that specifies the format of
 the input formula
 [@confiIsoElim@] enable/disable isomorphism elimination
 [@configSkolemDepth@] depth of Skolem terms for provenance information
-}

data Config = Config { configInput              :: Maybe String
                       -- input theory
                     , configDebug              :: Bool
                       -- debug mode (on/off)
                     -- , configIncremental     :: Bool
                       -- incremental view maintenance (on/off)
                     -- , configAllModels       :: Bool
                       -- return all models
                     -- , configBound           :: Maybe Int
                       -- maximum size of models
                     -- , configTPTPPath        :: String
                       -- the path to TPTP folder
                     -- , configInputType       :: InputType
                       -- type of the input formulas to process
                     -- , configIsoElim         :: Bool
                       -- eliminate isomorphic models
                     , configDefaultSkolemDepth :: Int
                       -- default depth of Skolem terms for bounding the search
                     , configSkolemDepth        :: SkolemDepthMap
                       -- customized Skolem depth for each Skolem function
                     , configRelaxMin           :: Bool
                       -- whether the resulting models are purely minimal or
                       -- the condition is relaxed
                     , configCommand            :: Maybe String
                       -- CLI UserSyntax command string
                     , configState              :: Maybe String
                       -- CLI State XML file path
                     }

instance Show Config where
    show cfg = "--input=" ++ showOptional (configInput cfg) ++ "\n" ++
               "--debug=" ++ show (configDebug cfg) ++ "\n" ++
               -- "--incremental=" ++ show (configIncremental cfg) ++ "\n" ++
               -- "--one=" ++ show ((not.configAllModels) cfg) ++ "\n" ++
               -- "--bound=" ++ 
               --                (case configBound cfg of
               --                   Nothing -> "unbounded"
               --                   Just b  -> show b) ++ "\n" ++
               -- "--tptp-path=" ++ (configTPTPPath cfg) ++ "\n" ++
               -- "--input-type=" ++ show (configInputType cfg) ++ "\n" ++
               -- "--iso-elim=" ++ show (configIsoElim cfg) ++ "\n" ++
               "--depth=" ++ show (configDefaultSkolemDepth cfg) ++ "\n" ++
               "--relax=" ++ show (configRelaxMin cfg) ++ "\n" ++ 
               "--command=" ++ showOptional (configCommand cfg) ++ "\n" ++
               "--state=" ++ showOptional (configState cfg)

-- Helper for the show
showOptional :: Show a => Maybe a -> String
showOptional Nothing  = "Not specified!"
showOptional (Just v) = show v


{-| Returns an instance of 'Config' with default values. -}
defaultConfig = Config { configInput              = Nothing 
                       , configDebug              = False
                       -- , configIncremental     = True
                       -- , configAllModels       = True
                       -- , configBound           = Nothing
                       -- , configTPTPPath        = "./"
                       -- , configInputType       = GeoLog
                       -- , configIsoElim         = False
                       , configDefaultSkolemDepth = -1
                       , configSkolemDepth        = emptySkolemDepthMap
                       , configRelaxMin           = False
                       , configCommand            = Nothing 
                       , configState              = Nothing }

{-| ConfigMonad is  a state monad with a 'Config' instance as state. -}
type ConfigMonad  = State Config

{-| ConfigMonad is  a state monad transformer with a 'Config' instance as 
  state. -}
type ConfigMonadT = StateT Config