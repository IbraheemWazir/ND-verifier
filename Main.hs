module Main where 

import Parser
import Lexer
import Verifier
import System.Environment
import Data.Maybe 

main = do 
     [f] <- getArgs
     lineList <- fmap lines (readFile f)
     let lines = getLineWords lineList
     print (lines)
     let lines' = filter (not . null) lines
     print(lines')
     let exps  = parseLines lines'
     print(exps)
     if isNothing exps 
         then 
             putStrLn "Parse Error"
     else do 
         let vExps = verify (fromJust exps)
         if isNothing vExps  
             then putStrLn "Proof is invalid"
             else putStrLn ("Proof is valid \n" ++ (show (fromJust vExps)))      
    
