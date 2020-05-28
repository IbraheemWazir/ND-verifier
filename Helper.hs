module Helper where 

import Data.Maybe
import Text.Read

listify :: [a] -> [[a]]
listify = map (\x -> [x])

-- checks whether a string can be read as an integer e.g. "1234"
isInt :: String -> Bool
isInt str =  isJust (readMaybe str :: Maybe Int)  

-- checks whether two lists have a common element
containsElem :: Eq a => [a] -> [a] -> Bool
containsElem xs = not . null . filter (`elem` xs)  

-- checks whether a key exists in a list of ordered pairs (key, val)
keyExists :: Eq a => a -> [(a,b)] -> Bool
keyExists key list = not(isNothing (lookup key list))

-- PRE: the key exists in the list
-- finds the value corresponding to the key in the list
lookUp :: Eq a => a -> [(a,b)] -> b
lookUp key list = fromJust (lookup key list)    

-- finds the most recent occurrance of a key in a list of lists of order pairs
-- outputs Nothing if not found, a Maybe instance of the val otherwise
lookupRecent :: Eq a => a -> [[(a,b)]] -> Maybe b
lookupRecent key [] = Nothing
lookupRecent key (l:ls) 
    | (isNothing lMaybe) = lookupRecent key ls
    | otherwise          = lMaybe
    where 
    lMaybe = lookup key l 

findAll :: Eq a => a -> [(a, b)] -> [b]
findAll x xs = [y | (x, y) <- xs]

getKeys :: [(a,b)] -> [a]
getKeys = map (\(x,y) -> x)
