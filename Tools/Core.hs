module Tools.Core where

import Chase.Problem.Model
import Chase.Problem.Provenance
import Chase.Problem.Observation
import Formula.SyntaxGeo

import Data.Maybe

squeeze :: Int -> Model -> Model
squeeze c mdl = case eq of
                  Nothing -> mdl
                  Just e  -> let (m', _, c') = add mdl c [e] UserProv
                             in  squeeze c' m'
    where eq = squeezeEquations' mdl
      
squeezeEquations :: Model -> [Obs]
squeezeEquations mdl = 
    [Eql (Elm e1) (Elm e2) | (e1, e2) <- pairs, greaterElem mdl e1 e2]
    where dom   = modelDomain mdl
          pairs = [(x, y) | x <- dom, y <- dom]

squeezeEquations' :: Model -> Maybe Obs
squeezeEquations' mdl = 
    listToMaybe [Eql (Elm e1) (Elm e2) | (e1, e2) <- pairs
                , greaterElem mdl e1 e2]
    where dom   = modelDomain mdl
          pairs = [(x, y) | x <- dom, y <- dom]