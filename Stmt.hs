module Stmt where 


import Expr 


type Block = [Stmt]


data Stmt 
    = Assign String Expr 
    | If Expr Block Block 
    | While Expr Block 
    | Block String [String] Block 