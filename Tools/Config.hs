module Tools.Config 
    (ScheduleType(..), 
     ConfigMonad, 
     Config, 
     defaultConfig, configDebug, configSchedule) where

import Control.Monad.State

data ScheduleType = SchedFIFO
                  | SchedFILO
data Config = Config { configDebug :: Bool
                     , configSchedule :: ScheduleType}

defaultConfig = Config False SchedFIFO

type ConfigMonad = State Config