module Tools.IConfig where

import Data.Maybe
import Control.Monad.State.Lazy as State

data ScheduleType = SchedBFS
                  | SchedDFS
                  | SchedRR
                    deriving Eq

instance Show ScheduleType where
    show SchedBFS = "bfs"
    show SchedDFS = "dfs"
    show SchedRR  = "rr" 

data FormulaType = GeoLog | TPTPCNF | TPTPFOF

instance Show FormulaType where
    show GeoLog  = "geo"
    show TPTPCNF = "cnf"
    show TPTPFOF = "fof"

data Config = Config { configInput       :: Maybe String
                       -- input theory
                     , configDebug       :: Bool
                       -- debug mode (on/off)
                     , configSchedule    :: ScheduleType
                       -- problem scheduling mode
                     , configBatch       :: Bool
                       -- batch processing (on/off)
                     , configIncremental :: Bool
                       -- incremental view maintenance (on/off)
                     , configAllModels   :: Bool
                       -- return all models
                     , configProcessUnit :: Int
                       -- processing unit when scheduling mode is round robin
                     , configBound       :: Maybe Int
                       -- maximum size of models
                     , configTPTPPath    :: String
                       -- the path to TPTP folder
                     , configFormulaType :: FormulaType
                       -- type of the formula to be processed
                     , configIsoElim     :: Bool
                       -- eliminate isomorphic models
                     , configSkolemDepth :: Int
                       -- depth of skolem term for reusing elements
                     }

instance Show Config where
    show cfg = "--input=" ++ show ((fromJust.configInput) cfg) ++ "\n" ++
               "--debug=" ++ show (configDebug cfg) ++ "\n" ++
               "--schedule=" ++ show (configSchedule cfg) ++ "\n" ++
               "--batch=" ++ show (configBatch cfg) ++ "\n" ++
               "--incremental=" ++ show (configIncremental cfg) ++ "\n" ++
               "--one=" ++ show ((not.configAllModels) cfg) ++ "\n" ++
               "--process-unit=" ++ show ((configProcessUnit) cfg) ++ "\n" ++
               "--bound=" ++ 
                              (case configBound cfg of
                                 Nothing -> "unbounded"
                                 Just b  -> show b) ++ "\n" ++
               "--tptp-path=" ++ (configTPTPPath cfg) ++ "\n" ++
               "--formula-type=" ++ show (configFormulaType cfg) ++ "\n" ++
               "--iso-elim=" ++ show (configIsoElim cfg) ++ "\n" ++
               "--skolem-depth=" ++ show (configSkolemDepth cfg)

defaultConfig = Config { configInput       = Nothing 
                       , configDebug       = False
                       , configSchedule    = SchedBFS
                       , configBatch       = False 
                       , configIncremental = False
                       , configAllModels   = True
                       , configProcessUnit = 20
                       , configBound       = Nothing
                       , configTPTPPath    = "./"
                       , configFormulaType = GeoLog
                       , configIsoElim     = False
                       , configSkolemDepth = -1 }

{- ConfigMonad is  a state monad with a Config instance as its state. -}
type ConfigMonad  = State Config
type ConfigMonadT = StateT Config

get :: State.MonadState s m => m s
get =  State.get

put :: State.MonadState s m => s -> m ()
put =  State.put

lift :: (Monad m, MonadTrans t) => m a -> t m a
lift = State.lift

