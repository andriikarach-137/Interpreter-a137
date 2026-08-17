module Stmt where 


import Expr 


data Stmt 
    = Declare String Type 
    | Assign String Expr 
    | Print Expr 