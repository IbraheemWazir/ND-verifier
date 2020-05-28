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
     let exps  = parseLines lines
     if isNothing exps 
         then 
             putStrLn "Parse Error"
     else do 
         let vExps = verify (fromJust exps)
         if isNothing vExps  
             then putStrLn "Proof is invalid"
             else putStrLn ("Proof is valid \n" ++ (show (fromJust vExps)))      
    
