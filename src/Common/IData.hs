{-# LANGUAGE Rank2Types #-}

{- Razor
   Module      : Common.IData
   Description : Implements common data types that are used by various layers
   of the program.
   Maintainer  : Salman Saghafi -}
module Common.IData where

-- Standard
import qualified Data.Map as Map

-- Syntax
import Syntax.GeometricUtils

-- Common
import Common.Basic

{-| The SequentLike type class captures types that represent geometric 
  'Sequent's in 'HerbrandBase'. This allows for different implementations of 
  'HerbrandBase' with different algorithms. 

  [@fromSequent@] creates a SequentLike instance for an input 'Sequent'. 
  [@toSequent@] returns the original 'Sequent' for a 'SequentLike' instance.
  [@skolemFunctions@] returns a list of Skolem functions assigned to the 
  existential quantifiers of an instance of type @s@.
  [@failSequent@] returns true if the head of the sequent is empty. 
  [@startSequent@] returns true if the body of the sequent is empty. -}
class (Show s) => SequentLike s where
    fromSequent      :: Sequent -> s
    toSequent        :: s -> Sequent
    skolemFunctions  :: s -> [FnSym]
    sequentConstants :: s -> [Constant]
    startSequent     :: s -> Bool
    failSequent      :: s -> Bool


{- 'Sequnet' is 'SequentLike' -}
instance SequentLike Sequent where
    toSequent        = id
    fromSequent      = id
    startSequent s   = (sequentBody s) == Tru
    failSequent  s   = (sequentHead s) == Fls
    skolemFunctions  = sequentExistentials
    sequentConstants = constants