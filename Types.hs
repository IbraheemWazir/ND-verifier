module Types where

import Helper 

data Quantifier = Forall | ThereExists
    deriving(Eq, Ord, Show)
data EqOp = Equals | Geq | Leq | GreaterThan | LessThan
    deriving(Eq, Ord, Show)
data BinBool = And | Or | Implies | Iff 
    deriving(Eq, Ord, Show)
data UnBool = Not
    deriving(Eq, Ord, Show)
data Command = Given | Ass | Const | None
    deriving(Eq, Ord, Show)
data Exp = Var String | TF String | BinApp BinBool Exp Exp | UnApp UnBool Exp | QApp Quantifier String Exp
    deriving(Eq, Ord)

type Env = [[(Exp, Command)]]

instance Show Exp where
    show = toString

eqChars = ['=', '<', '>']
eqStrs  = listify eqChars
opChars = ['-', '+', '*', '/']
opStrs  = listify opChars
invalidChars = ['.', ',']
eolChar = [';']
parChars = ['(', ')']
parStrs = listify parChars
specialChars = eqChars ++ opChars ++ invalidChars ++ eolChar ++ parChars
specialChars' = eqChars ++ opChars ++ invalidChars ++ eolChar
specialStrs' = [">=", "<=", "<->", "->"]
specialStrs = (listify specialChars) ++ specialStrs' 

binBoolStrs :: [(BinBool, String)]
binBoolStrs = [(And, " and "),  (Or, " or "), (Implies, " -> "), (Iff, " <-> ")]

unBoolStrs :: [(UnBool, String)]
unBoolStrs = [(Not, "not ")]

quantifiers = [(Forall, "forall"), (ThereExists, "thereExists")]

toString :: Exp -> String
toString (Var var)   = var
toString (TF b)      = b
toString (BinApp boolOp exp1 exp2) 
    = (toString exp1) ++ (lookUp boolOp binBoolStrs) ++ (toString exp2)
toString (UnApp boolOp exp)
    = (lookUp boolOp unBoolStrs) ++ (toString exp)
toString (QApp q b exp) 
    = (lookUp q quantifiers) ++ b ++ "(" ++ (toString exp) ++ ")"
