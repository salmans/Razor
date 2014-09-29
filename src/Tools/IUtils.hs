{- Razor
   Module      : Tools.IUtils
   Description : Implements helper functions.
   Maintainer  : Salman Saghafi -}

module Tools.IUtils where

-- Standard
import Data.Char (isSpace)
import Data.List (nub)

{-| Computes the union of a list of lists. -}
unions :: (Ord a) => [[a]] -> [a]
unions ls = nub $ foldr (++) [] ls

{-| Given an input string (corresponding to a line in the theory file), returns
  True if the line is not commented out and is not blank. -}
isRealLine :: String -> Bool
isRealLine l = 
    case l of
      [] -> False
      ('-':'-':_) -> False
      otherwise -> any (not.isSpace) l
