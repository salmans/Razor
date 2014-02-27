module Utility ((!!!)) where

(!!!) :: [a] -> Int -> Maybe a
xs !!! index = if index < 0 then Nothing else getIndex xs index

getIndex :: [a] -> Int -> Maybe a
getIndex xs index = case xs of
  [] -> Nothing
  first:rest
    | index == 0 -> Just first
    | otherwise -> getIndex rest $ pred index