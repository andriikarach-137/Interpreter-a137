module Stmt where 


import Expr 


type Block = [Stmt]


data Stmt 
    = Assign String Expr 
    | Print Expr 