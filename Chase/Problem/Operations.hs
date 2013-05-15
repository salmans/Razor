{-| Time-stamp: <2013-05-15 12:38:54 Salman Saghafi>

   This module contains basic operations related to a problem structure. 

-}
module Chase.Problem.Operations (buildProblem, 
                                 extendProblem,
                                 selectProblem, 
                                 scheduleProblem,
                                 holds,
                                 formulaHolds,
                                 sequentHolds,
                                 narrowObs) where

import Chase.Problem.IOperations