module Tools.IConfig where

import Data.Maybe
import Control.Monad.State

data ScheduleType = SchedBFS
                  | SchedDFS
                  | SchedRR
                    deriving Eq

instance Show ScheduleType where
    show SchedBFS = "bfs"
    show SchedDFS = "dfs"
    show SchedRR  = "rr" 

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
                     }

instance Show Config where
    show cfg = "--input=" ++ show ((fromJust.configInput) cfg) ++ "\n" ++
               "--debug=" ++ show (configDebug cfg) ++ "\n" ++
               "--schedule=" ++ show (configSchedule cfg) ++ "\n" ++
               "--batch=" ++ show (configBatch cfg) ++ "\n" ++
               "--incremental=" ++ show (configIncremental cfg) ++ "\n" ++
               "--one=" ++ show ((not.configAllModels) cfg) ++ "\n" ++
               "--process-unit=" ++ show ((configProcessUnit) cfg) ++ "\n"


defaultConfig = Config Nothing False SchedBFS False False True 20

type ConfigMonad = State Config