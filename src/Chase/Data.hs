{-| 
  Razor
  Module      : Chase.Data
  Description : Chase.Data provides the basic data-structures that are used by
  the Chase algorithm.
  Maintainer  : Salman Saghafi -}

module Chase.Data ( PossibleFacts (..) , ChaseImpl (..)
                  , SequentMap, buildSequentMap
                  , Problem (..), buildProblem
                  , problemSequentMap, problemBase, problemDelta, problemProvs
                  , problemSATTheory
                  , PushM, runPushM,  evalPushM
                  , liftPushMBase, liftPushMProvs, liftPushMCounter
                  , liftPushMSATTheory, liftPushMConfig
                  , PullM, runPullM, evalPullM, liftPullMBase, liftPullMProvs
                  , ChaseM (..), liftChaseMState, liftChaseMCounter
                  , liftChaseMConfig
                  , incompleteSequent ) where

import Chase.IData