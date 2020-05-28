module Rules where

import Types
import Helper
import Data.Maybe

expTrue :: Exp -> Env -> Bool
expTrue exp env = isNothing $ lookupRecent exp env

addToEnv :: Env -> [Exp] -> Env
addToEnv env [] = env
addToEnv (b:bs) (e:es)
    = addToEnv (((e,None):b):bs) es


-- PRE: expTrue exp env
getCommand :: Exp -> Env -> Command
getCommand exp env = fromJust $ lookupRecent exp env
findOrs :: Exp -> Env -> [Exp]
findOrs exp env
    = findOrs' exp env []
    where 
    findOrs' :: Exp -> Env -> [Exp] -> [Exp]
    findOrs' exp ((((BinApp Or e1 e2), _):b):bs) es 
        | e1 == exp              = findOrs' exp (b:bs) (e2:es)
        | e2 == exp              = findOrs' exp (b:bs) (e1:es)
        | otherwise              = findOrs' exp (b:bs) es
    findOrs' exp ([]:bs) es                       = findOrs' exp bs es
    findOrs' exp ((e:b):bs)  es                   = findOrs' exp (b:bs) es 
    findOrs' exp [] es                            = es
findIfs :: Exp -> Env -> [Exp]
findIfs exp env
    = findIfs' exp env []
    where
    findIfs' :: Exp -> Env -> [Exp] -> [Exp]
    findIfs' exp ((((BinApp Implies e1 e2), _):b):bs) es 
        | e1 == exp = findIfs' exp (b:bs) (e2:es)
        | otherwise = findIfs' exp (b:bs) es
    findIfs' exp ([]:bs) es                       = findIfs' exp bs es
    findIfs' exp ((e:b):bs)  es                   = findIfs' exp (b:bs) es
    findIfs' exp [] es                            = es

-- for input p, returns true if not p exists in the environment, else false
hasNot :: Exp -> Env -> Bool
hasNot e [] = False
hasNot e (((UnApp Not exp, _):b):bs) 
    | e == exp = True
    | otherwise = hasNot e (b:bs)
hasNot e ([]:bs) = hasNot e bs
hasNot e ((a:b):bs) = hasNot e (b:bs)  

-- for input 
-- andIntro 
-- p true and q true -> (p and q) true
andIntro :: Exp -> Exp -> Env -> Maybe Env
andIntro exp1 exp2 [] = Nothing
andIntro exp1 exp2 env@(b:bs)
    | andValid  = Just  (((andExp, None):b):bs)
    | otherwise = Nothing
    where 
    andValid :: Bool
    andExp   :: Exp
    andValid = and $ map (`expTrue` env) [exp1, exp2]
    andExp   = BinApp And exp1 exp2

-- andElimination
-- p and q -> (p true) and (q true)
andApply :: Exp -> Exp -> Env -> Env
andApply exp1 exp2 [] = [] -- shouldn't be reachable
andApply exp1 exp2 (b:bs) 
	= ((exp1, None):(exp2, None):b):bs

-- orIntro
-- p true -> p or q true
-- q true -> q or p true
orIntro :: Exp -> Exp -> Env -> Maybe Env
orIntro exp1 exp2 [] = Nothing
orIntro exp1 exp2 env@(b:bs)  
    | orValid   = Just (((orExp, None):b):bs)
    | otherwise = Nothing
    where
    orValid = or $ map (`expTrue` env) [exp1, exp2] 
    orExp   = BinApp Or exp1 exp2



-- orElimination
-- (p -> r) and (q -> r) and (p or q) 
orElim :: Exp -> Exp -> Env -> Env
orElim p q [] = []
orElim p q env@(b:bs) 
    | null ifs  = env
    | otherwise = addToEnv env ifs
    where 
    ifs'  = findIfs p env
    ifs'' = findIfs q env
    ifs   = [x | x <- ifs', elem x ifs'']
orApply :: Exp -> Exp -> Env -> Env
orApply p r  [] = [] -- shouldn't be reachable
orApply p r env@(b:bs)
    = if cond 
      then ((r, None):b):bs
      else env
     where 
     ors = findOrs p env
     cond = not $ null [q | q <- ors, expTrue (BinApp Implies q r) env]

-- notIntro
--     A ass
--      ...
--     "False"
-- not A
notIntro :: Exp -> Env -> Maybe Env
notIntro exp []  = Nothing
notIntro exp [_] = Nothing
notIntro exp@(UnApp Not e) env@(a:b:bs)
    | cond      = Just (((e, None):((UnApp Not exp), None):b):bs)
    | otherwise = Nothing
    where
    cond'  = keyExists exp a
    cond'' = keyExists (TF "False") a
    cond = cond' && cond'' && (getCommand exp env) == Ass
notIntro exp env@(a:b:bs)
    | cond      = Just ((((UnApp Not exp), None):b):bs) 
    | otherwise = Nothing
    where 
    cond'  = keyExists exp a
    cond'' = keyExists (TF "False") a
    cond = cond' && cond'' && (getCommand exp env) == Ass

-- -> Intro
--     p   ass
--      ...
--     q
-- p -> q
ifIntro :: Exp -> Exp -> Env -> Maybe Env
ifIntro exp1 exp2 []  = Nothing
ifIntro exp1 exp2 [_] = Nothing
ifIntro exp1 exp2 env@(a:b:bs) 
    | cond      = Just ((((BinApp Implies exp1 exp2), None):b):bs) 
    | otherwise = Nothing
    where 
    cond'  = keyExists exp1 a
    cond'' = keyExists exp2 a  
    cond = cond' && cond'' && (getCommand exp1 env) == Ass

iffIntro :: Exp -> Exp -> Env -> Maybe Env
iffIntro _ _ []  = Nothing
iffIntro _ _ [_] = Nothing
iffIntro exp1 exp2 env
      = if isNothing env' 
      then Nothing
      else ifIntro exp2 exp1 (fromJust env')
    where
    env' = ifIntro exp1 exp2 env 

ifApply :: Exp -> Exp -> Env -> Env
ifApply _ _ [] = [] -- not reachable
ifApply p q env@(b:bs) 
    | expTrue p env = (((q,None):b):bs)
    | otherwise     = env

iffApply :: Exp -> Exp -> Env -> Env
iffApply p q env = ifApply p q (ifApply q p env)
notApply :: Exp -> Env -> Env
notApply (UnApp Not exp) env@(b:bs) 
    | expTrue exp env = ((TF "False", None):b):bs
    | otherwise       = env   
