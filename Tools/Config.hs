module Tools.Config ( ScheduleType (..)
                    , FormulaType (..)
                    , Config (..)
                    , ConfigMonad, get, put, lift
                    , defaultConfig) where

import Tools.IConfig hiding (Config.Config)