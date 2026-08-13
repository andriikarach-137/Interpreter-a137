module Expr where 


import Data.Map (Map)
import Data.Map qualified as Map 


type Env = Map String Val 


data Type 
    = Int 
    | Real 
    | Char 
    | String 
    | Tuple Type Type 
    | List Type 
    | Fun Type Type 


data Val 
    = VInt Int 
    | VReal Double 
    | VChar Char 
    | VString String 
    | VTuple (Val, Val)
    | VList [Val]
    | VFun String Expr Env 


data Expr 
    = Lit Val 
    | UnOp UnOp Expr 
    | BinOp BinOp Expr Expr 
    | Apply Val Expr 
    | If Expr Expr Expr 


data BinOp 
    = Add | Sub | Mul | Div | Pow | IntDiv | Mod 
    | And | Or | Xor 
    | Eq | NEq | LT | GT | LE | GE 
    | Concat | Cons 


data UnOp
    = Inc | Decr | Neg | Fact 
    | Not
    | Log | Exp | Sin | Cos | Tan | ASin | ACos | ATan 
    | Head | Tail 
    | Fst | Snd 