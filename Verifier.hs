module Verifier where
import Helper
import Types
import Rules
import Data.Maybe
successMsg = "Well done, proof is correct"
constErrMsg = "Constant not in correct format" 
verify :: [(Exp, Command)] -> Maybe [Exp]
verify input 
    = verify' input []
    where 
    verify' :: [(Exp, Command)] -> Env -> Maybe [Exp]
    verify' [] env 
        = Just (getKeys b)
        where
        (b:bs) = reverse env  
    verify' ((exp, Given):es) []  = verify' es (applyRule exp [[(exp, Given)]])
    verify' ((exp, Given):es) env = verify' es env''
        where 
        (b:bs) = reverse env
        env'   = reverse ((exp, Given):b):bs
        env''  = applyRule exp env' 
    
    verify' ((exp, Ass):es) []  = verify' es (applyRule exp [[(exp, Ass)], []])
    verify' ((exp, Ass):es) env = verify' es env''
        where 
        env'  = [(exp, Ass)]:env
        env'' = applyRule exp env
    
    verify' ((Var c, Const):es) [] = verify' es (applyRule (Var c) [[(Var c, Const)], []])
    verify' ((Var c, Const):es) env = if expTrue (Var c) env
                                    then Nothing
                                    else verify' es env'
        where 
        env' = [(Var c, Const)]:env
    
    verify' ((exp, Const):es) _   = Nothing 
    
    verify' ((exp, None):es) env  
        = if isNothing envMaybe
          then Nothing
          else verify' es (applyRule exp (fromJust envMaybe))
        where 
            envMaybe = checkRule exp env

    
applyRule :: Exp -> Env -> Env
applyRule e@(Var x) env                = notIfCheck e env
applyRule (TF b) env                   = env -- need to make rule for falseElim in checkRule
applyRule e@(BinApp And e1 e2) env     = notIfCheck e (andApply e1 e2 env)
applyRule e@(BinApp Or e1 e2) env      = notIfCheck e (orElim e1 e2 env)
applyRule e@(BinApp Implies e1 e2) env = notIfCheck e (ifApply e1 e2 (orApply e1 e2 env))
applyRule e@(BinApp Iff e1 e2) env     = notIfCheck e (iffApply e1 e2 (orApply e1 e2 (orApply e2 e1 env)))
applyRule e@(UnApp Not e1) env         = notIfCheck e (notApply e env)


checkRule :: Exp -> Env -> Maybe Env
checkRule exp [] = Nothing
checkRule exp env@(b:bs)
    | expTrue exp env        = Just (((exp, None):b):bs)
    | cond' && cond          = if null bs
                               then Just [[(exp, None)]]
                               else Just (((exp,None):c):cs) 
    | cond                   = Just (((exp, None):b):bs) -- FE
    | otherwise              = checkRule' exp env
    where 
    (a:c:cs) = env 
    cond = keyExists (TF "False") b
    cond' = keyExists (UnApp Not exp) b 
    checkRule' :: Exp -> Env -> Maybe Env 
    checkRule' (BinApp And e1 e2) env      = andIntro e1 e2 env   
    checkRule' (BinApp Or e1 e2) env       = orIntro e1 e2 env
    checkRule' (UnApp Not e) env           = notIntro e env
    checkRule' (BinApp Implies e1 e2) env  = ifIntro e1 e2 env
    checkRule' (BinApp Iff e1 e2) env      = iffIntro e1 e2 env
    checkRule' _ env                       = Nothing

ifCheck :: Exp -> Env -> Env
ifCheck e@(BinApp And e1 e2) env
    = addToEnv env ifs
    where
    ifs =  (findIfs e env) ++ (findIfs e1 env) ++ (findIfs e2 env)
ifCheck e env
     = addToEnv env (findIfs e env) 

notIfCheck :: Exp -> Env -> Env
notIfCheck e@(BinApp And e1 e2) env@(b:bs)
    | cond      = ifCheck e env
    | otherwise = ifCheck e (((TF "False", None):b):bs)
    where
    cond = null [y | y <- [e, e1, e2], hasNot y env]

notIfCheck e env@(b:bs)
    | hasNot e env  = ifCheck e (((TF "False", None):b):bs)
    | otherwise     = ifCheck e env   
