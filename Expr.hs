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
    | Tuple Type Type 
    | List Type 
    | Fun Type Type 


-- Each type has some sort of runtime value 
data Val 
    = VInt Int 
    | VReal Double 
    | VChar Char 
    | VString String 
    | VTuple (Val, Val)
    | VList [Val]
    | VFun String Expr Env 


-- Expression is built from literal values, unary and binary operations, function applications, and if-then-else expressions
data Expr 
    = Lit Val 
    | Var String 
    | UnOp UnOp Expr 
    | BinOp BinOp Expr Expr 
    | Apply Val Expr 
    | If Expr Expr Expr 


-- Represents all sorts of binary operations 
data BinOp 
    = Add | Sub | Mul | Div | Pow | IntDiv | Mod 
    | And | Or | Xor 
    | Eq | NEq | LT | GT | LE | GE 
    | Concat | Cons 


-- Represents all sorts of unary operations 
data UnOp
    = Inc | Decr | Neg | Fact 
    | Not
    | Log | Exp | Sin | Cos | Tan | ASin | ACos | ATan 
    | Head | Tail 
    | Fst | Snd 