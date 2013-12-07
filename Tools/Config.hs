module Tools.Config 
    (ConfigMonad, Config, defaultConfig, configDebug) where

import Control.Monad.State

-- Salman: Debugging ideas:
-- add verbosity level
-- use the writer monad in ProbPool

data Config = Config { configDebug :: Bool}

defaultConfig = Config False

type ConfigMonad = State Config