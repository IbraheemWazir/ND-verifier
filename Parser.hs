module Parser where

import Types
import Helper
import Data.Maybe

comsList:: [(String, Command)]
comsList = [("given", Given), ("ass", Ass) , ("const", Const)]
boolList :: [String]
boolList = ["True", "False"]
precedence :: [(String, Int)]
precedence = [("not", 1), ("and", 2), ("or", 3), ("->", 4), ("<->", 5), ("forall", 6), ("thereExists", 7), ("(", 8)]

workoutExp :: [Exp] -> String -> Maybe [Exp]
workoutExp (e1 : e2 : es) "and"               = Just  ((BinApp And e2 e1) : es)
workoutExp (e1 : e2 : es) "or"                = Just ((BinApp Or e2 e1) : es)
workoutExp (e1 : e2 : es) "->"                = Just ((BinApp Implies e2 e1) : es)
workoutExp (e1 : e2 : es) "<->"               = Just ((BinApp Iff e2 e1) : es)
workoutExp (e1 : es) "not"                    = Just ((UnApp Not e1) : es)
workoutExp (e1 : (Var b)  : es) "forall"      = Just ((QApp Forall b e1) : es)
workoutExp (e1 : (Var b)  : es) "thereExists" = Just ((QApp ThereExists b e1) : es)  
workoutExp _ _                                = Nothing

parse :: [String] -> Maybe (Exp, Command)
parse words 
    = parse' (filter (not . null) words) [] [] []
    where 
    parse' :: [String] -> [Exp] -> [String] -> [Command] -> Maybe (Exp, Command)
    parse' [";"] [x] [] [com] = Just (x, com)
    parse' [";"] [x] [] []    = Just (x, None)
    parse' [";"] [] _ _     = Nothing
    parse' [";"] expStack (o:os) coms = if (isNothing w) then Nothing
                                        else parse' [";"] (fromJust w) os coms 
        where 
        w = workoutExp expStack o
    parse' [_] _ _ _ = Nothing
    parse' ("(" :xs) expStack opStack coms 
        = parse' xs expStack ("(":opStack) coms
    parse' (")" : xs) expStack [] _ = Nothing
    parse' (")" : xs) expStack (o : os) coms
        | o == "(" = parse' xs expStack os coms
        | otherwise = if isNothing w
                      then Nothing
                      else parse' (")":xs) (fromJust w) os coms
        where
        w = workoutExp expStack o
    
    parse' (x : xs) expStack [] coms 
        | keyExists x precedence = parse' xs expStack [x] coms
        | keyExists x comsList && null coms && null expStack = parse' xs expStack [] [com]
        | keyExists x comsList         = Nothing
        | elem x boolList              = parse' xs ((TF x):expStack) [] coms
        | otherwise                    = parse' xs ((Var x):expStack) [] coms
        where 
        com = lookUp x comsList
    parse' (x : xs) expStack ops@(o : os) coms
        | keyExists x precedence = if (lookUp x precedence > lookUp o precedence && o /= "(")
                                  then if (isNothing w) then Nothing
                                       else parse' (x:xs) (fromJust w) os coms
                                  else parse' xs expStack (x:ops) coms
        | keyExists x comsList              = Nothing
        | elem x boolList              = parse' xs ((TF x):expStack) ops coms
        | otherwise                    = parse' xs ((Var x):expStack) ops coms
        where
        w = workoutExp expStack o 
        com = lookUp x comsList

parseLines :: [[String]] -> Maybe [(Exp, Command)]
parseLines lines 
    = parseLines' lines  []
    where
    parseLines' :: [[String]] -> [(Exp, Command)] -> Maybe [(Exp, Command)]
    parseLines' [] es = Just es
    parseLines' (x:xs) es 
        = if isNothing parsedStr 
          then Nothing
          else parseLines' xs (es ++ [fromJust parsedStr]) 
        where
        parsedStr = parse x
