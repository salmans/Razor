{-| 

   This module contains basic operations related to a problem structure. 

-}
module Chase.Problem.Operations (buildProblem, 
                                 extendProblem,
                                 selectProblem, 
                                 scheduleProblem,
                                 holds,
                                 formulaHolds,
                                 sequentHolds,
                                 matchFrame
                                ) where

import Chase.Problem.IOperations