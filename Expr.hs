module Expr where 


import Data.Map (Map)
import Data.Map qualified as Map 


-- Program has variables that store values, so we introduce map of bindings 
type Env = Map String Val 


-- Our language is statically typed, thus we need some types 
data Type 
    = Int 
    | Real 
    | Char 
    | String 
    | Pair Type Type 
    | List Type 
    | Dictionary Type Type 
    | Fun Type Type 


-- Each type has some sort of runtime value 
data Val 
    = VInt Int 
    | VReal Double 
    | VChar Char 
    | VString String 
    | VPair (Val, Val)
    | VList [Val]
    | VDictionary (Map Val Val) 
    | VFun String Expr Env 
    deriving (Eq, Ord)
 

-- Expression is built from literal values, unary and binary operations, function applications, and if-then-else expressions
data Expr 
    = Lit Lit 
    | Var String 
    | UnOp UnOp Expr 
    | BinOp BinOp Expr Expr 
    | If Expr Expr Expr 
    deriving (Eq, Ord)


-- Represents literal expressions 
data Lit 
    = LInt Int 
    | LReal Double 
    | LChar Char 
    | LString String 
    | LPair (Expr, Expr)
    | LList [Expr]
    | LDictionary (Map Expr Expr)
    | LFun String Expr
    deriving (Eq, Ord)


-- Represents all sorts of binary operations 
data BinOp 
    = Add | Sub | Mul | Div | Pow | IntDiv | Mod 
    | And | Or | Xor 
    | Eq | NEq | LT | GT | LE | GE 
    | Concat | Cons 
    | Apply
    deriving (Eq, Ord)

-- Represents all sorts of unary operations 
data UnOp
    = Inc | Dec | Neg | Fact 
    | Not
    | Log | Exp | Sin | Cos | Tan | ASin | ACos | ATan 
    | Head | Tail 
    | Fst | Snd 
    deriving (Eq, Ord)