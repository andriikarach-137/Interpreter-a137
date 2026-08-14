module Parser where 


import ParseUtils 


-- BNF 

{-

<program> ::= (<statement> SEMI)*
<stmt> ::= <assign> | <print> NL 

<assign> ::= <type> <ident> LARR <expr> NL
<print> ::= PRINT <expr> 
-}