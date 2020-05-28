module Lexer where
import Data.Char
import Helper
import Types

getWords :: String -> [String]
getWords cs 
	= getWords' cs [] []
	where 
	getWords' :: String -> String -> [String] -> [String]
	getWords' [] str res = (res ++ [str])
	getWords' (x : xs) str res
		| isSpace x      = getWords' xs [] (res ++ [str])
		| elem x specialChars = if ((containsElem specialChars str) || (null str))
					then getWords' xs (str ++ [x]) res
					else getWords' xs [x] (res ++ [str])
		| otherwise      = if containsElem specialChars str
					then getWords' xs [x] (res ++ [str])
					else getWords' xs (str ++ [x]) res		
getLineWords :: [String] -> [[String]]
getLineWords = map getWords	  
