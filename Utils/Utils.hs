{-# LANGUAGE NoMonomorphismRestriction #-}

module Utils.Utils where

import Data.Char
import Data.List
--import Data.Maybe
import Debug.Trace
--import Test.QuickCheck
--import Control.Exception -- for assert
import qualified Data.Map 
import qualified Data.Set 

 -- for the hashItfunction
import Data.HashTable
import GHC.Int 

-- ========================
-- Tracing
-- ========================

-- TODO: arrange to set this via command-line option
traceFlag = True
--traceFlag = False

traceFlag1 = True
--traceFlag1 = False

traceFlag2 = True
--traceFlag2 = False

trc :: String -> a -> a
trc = if traceFlag then trace else  (\_ x -> x) 

trc1  :: String -> a -> a
trc1 = if traceFlag1 then trace else  (\_ x -> x) 

trc2 = if traceFlag2 then trace else  (\_ x -> x) 

debug :: a -> String -> a
debug = flip trc



-- ==========================
-- Some pretty-printing tools
-- ==========================

flattenListString :: String -> String
flattenListString  = filter (\c -> c /= '[' && c /= ']' && c /= '\"') 

-- | Use this to strip off "[" and "]" and remove "\" infront of chars
unbracket :: String -> String
unbracket cs = init $ tail (filter (\c -> c /= '\"') cs)

-- | Make a list of strings look like a set
swapSquareCurly :: String -> String
swapSquareCurly ss = let bareList = unbracket ss 
                     in "{" ++ bareList ++ "}"

-- | Shows a formula with a binary op as the outermost structure.
-- showBinFormula :: String -> Formula -> Formula -> String
showBinFormula :: (Show a ) => String -> a -> a -> String
showBinFormula op f g = f' ++ " " ++ op ++ " " ++ g'
    where f' = addParens $ show f 
          g' = addParens $ show g

-- | Surrounds the string with parens.
addParens :: String -> String
addParens s = "(" ++ s ++ ")"

-- | Delimit list contents with commas.
commas :: [String] -> String
commas = intercalate ","

-- | Shows the list, comma separated and surrounded with parens instead of
-- brackets.
showWithParens :: Show t => [t] -> String
showWithParens = addParens . intercalate ", " . map show


-- Show a list one element per line
prettyList :: (Show a) => [a] -> IO ()
prettyList = putStr . linesList

-- 
linesList :: Show a => [a] -> String
linesList l = 
    let rows = map show l
    in  (unlines rows)



-- an integer representation of an item
hashIt :: Show a => a -> GHC.Int.Int32
hashIt t = Data.HashTable.hashString (show t)


{- Utilities for processing input files
-}
isNonEmptyLine :: [Char] -> Bool
isNonEmptyLine = any (not.isSpace)

isRealLine :: [Char] -> Bool
isRealLine l = 
    case l of
      [] -> False
      ('-':'-':_) -> False
      otherwise -> any (not.isSpace) l
      --`debug` ("isCommentLine: " ++ l)


-- qc = quickCheck -- need the NoMonomorphismRestriction

--
-- iterating functions
--

composeEm :: (a -> b -> b) -> [a] -> b -> b
{-
  if (f a) gives a (b->b) function, and l is a set of as,
  this composes all the functions 
-}
composeEm f l = foldl (.) id  (map f l)

-- test
blah = composeEm (\a -> \b -> a +b) [1,3,5] 17

-- diagrammatic-order composition
andThen :: (a -> b) -> (b -> c) -> a -> c
andThen = flip (.)



-- the famous 3n+1 function 
threeN :: Integer -> Integer
threeN 1 = 0
threeN x
  | even x    = 1 + threeN (x `div` 2)
  | otherwise = 1 + threeN (3*x+1)

repeatInt :: Int -> (a ->a) ->(a ->a)
-- repeat a fixed number of times
repeatInt n f x = if (n==0) then x else f (repeatInt (n-1) f x)

repeatTill :: (a -> Bool) -> (a ->a) ->(a -> a)
-- iterate function f until a result satisfies condition p
-- may loop forever!
repeatTill p f x =  case (find p (iterate f x)) of
         Just y -> y
         Nothing -> error "repeatTill"
                        
repeatTillNoChange :: Eq a => (a -> a) -> (a -> a)
-- iterate function f until a fixed point
-- may loop forever!
repeatTillNoChange f x = 
    case (findFirstRepeat (iterate f x)) of
         Just y -> y
         Nothing -> error "repeatTillNoChange"

-- will loop forever if no repeats
findFirstRepeat :: Eq a => [a] -> Maybe a
findFirstRepeat [] = Nothing 
findFirstRepeat [_]= Nothing
findFirstRepeat (x:y:rest) = if (x==y) then Just x else (findFirstRepeat (y:rest))


lft :: (a -> [a]) -> ([a] ->[a])
lft = concatMap

repeatIntLift :: Int -> (a -> [a]) -> (a -> [a])
repeatIntLift n f x = repeatInt n (lft f) [x]

composeList :: [(a->a)] -> (a->a)
-- compose the list of functions, in "diagram-order", ie head-function is first
composeList [] = (\x -> x)
composeList (f:fs) = (composeList fs) . f

composeNonDet :: (a ->[b]) -> (b -> [c]) -> (a -> [c])
composeNonDet f g x = concat [ (g y) | y <- (f x)]

composeListNonDet :: [(a -> [a])] -> (a -> [a])
composeListNonDet [] = (\x -> [x])
composeListNonDet (f :fs) = composeNonDet (composeListNonDet fs) f

noRepeats :: Eq a => [a] -> Bool
noRepeats ls = ( (length ls) == (length (nub ls)) )

composeListMaybe :: [(a -> (Maybe a))] -> a -> Maybe a
-- compose the list of functions, in "diagram-order", ie head-function is first
-- if any result is Nothing then so is the composition
composeListMaybe [] x = Just x
composeListMaybe (f:fs) x = 
    case ((composeListMaybe fs) x) of
      Just y -> case (f y) of
                  Just z -> Just z
                  Nothing -> Nothing
      Nothing -> Nothing



-- first elements in list1 a subset of the second?
listSubsetOf :: Ord a => [a] -> [a] -> Bool
listSubsetOf l1 l2 =
    let s1 = Data.Set.fromList l1
        s2 = Data.Set.fromList l2
    in Data.Set.isSubsetOf s1 s2

-- real "set theory" unions: the union of a set of sets
setConcat :: Ord a => Data.Set.Set (Data.Set.Set a) -> Data.Set.Set a
setConcat = (Data.Set.unions).Data.Set.toList

-- concat without duplicates
listSetConcat :: Eq a => [[a]] -> [a]
listSetConcat = nub.concat


allApps :: (a -> b -> [c]) -> [a] -> [b] -> [c]
-- all combinations of function f  on l1 and l2
allApps f l1 l2 = concat [f m1 m2 | m1 <- l1, m2<- l2]  


-- all maps between two lists
allMaps :: Ord a => [a] -> [b] ->[Data.Map.Map a b]
allMaps [] _ = [Data.Map.empty]
allMaps (v:vs) es =
    let 
        -- where can v go?
        allFirstPairs = map (\e -> (v,e)) es 
        -- map all but first                        
        tmpMaps = allMaps vs es   
    in [Data.Map.insert (fst p) (snd p) q | p <- allFirstPairs, q <- tmpMaps]


{- no need, just use list comprehension
-- All mappings as (association lists) between dom and ran 
allAssocLists:: [v] -> [e] ->[ [(v,e)] ]
allAssocLists [] _ =  [[]]
allAssocLists _ [] =  []
allAssocLists (hd:tl) ran = let tailResult = allAssocLists tl ran 
                                hdBindings = aListPair hd ran
                            in prodlist hdBindings tailResult
-}

-- The "product" of two lists, where the operator is cons 
-- input [a1;a2;..]  [l1;l2;...]  
-- output [ a1::l1; a1::l2; ... a2::l1;  a2::l2; .. ] 
prodList :: [[t]] -> [[t]] -> [[t]]
prodList l [] = l
prodList [] l = l
prodList l1 l2 = [x ++ y | x <- l1, y <- l2]


mapCons :: a -> [[a]] -> [[a]] 
mapCons item  =  map (\x -> item:x)  -- Here l is a list of lists, we cons item onto each

aListPair :: t -> [a] -> [(t, a)]
aListPair item  =  map (\x -> (item, x))  -- Here l is list, we add pair item with each elt of l 

-- allPairs f xs ys
-- returns f applied to all pairs of an element of xs with an element of ys
-- 
allPairs :: (a -> b -> c) -> [a] -> [b] -> [c]
allPairs f xs ys = [f x y | x <- xs, y <- ys]

-- returns all sublists of a list
allSublists :: [a] -> [[a]]
allSublists [] = [[]]
allSublists (x:xs) = [x:sublist | sublist <- allSublists xs] ++ allSublists xs

allCombinations :: [[a]] -> [[a]]
allCombinations [] = [[]]
allCombinations (x:xs) = [x':xs' | x' <- x, xs' <- allCombinations xs]
                         ++ allCombinations xs

-- retruns the result of applying 'n' cartesian products to a list
listPower :: [a] -> Int -> [[a]]
listPower list 0 = [[]]
listPower list 1 = [list]
listPower list 2 = [x:[y]| x <- list, y <- list]
listPower list n = [x:y| x <- list, y <- listPower list (n - 1)]

maybeHead :: [a] -> Maybe a
maybeHead []     = Nothing
maybeHead (x:xs) = Just x