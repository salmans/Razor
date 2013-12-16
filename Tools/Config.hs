module Tools.Config 
    (ScheduleType(..), 
     ConfigMonad, 
     Config, 
     defaultConfig, 
     configDebug, configSchedule, configBatch, configIncremental) where

import Control.Monad.State

data ScheduleType = SchedFIFO
                  | SchedFILO
data Config = Config { configDebug       :: Bool
                       -- debug mode (on/off)
                     , configSchedule    :: ScheduleType
                       -- problem scheduling mode
                     , configBatch       :: Bool
                       -- batch processing (on/off)
                     , configIncremental :: Bool
                       -- incremental view maintenance (on/off)
                     }

defaultConfig = Config False SchedFIFO False False

type ConfigMonad = State Config