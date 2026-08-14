module Parser where 


import ParseUtils 


-- BNF 

{-

<program>    ::= (<stmt> SEMI)*
<stmt>       ::= (<assign> | <print>) 

<assign>     ::= <type> <ident> LARR <expr>
<print>      ::= PRINT <expr> 

<expr>       ::= <or> | <or> CONS <expr> 
<or>         ::= <xor> (OR <xor>)*
<xor>        ::= <and> (XOR <and>)* 
<and>        ::= <comp> (AND <comp>)* 
<comp>       ::= <concat> | <concat> (EQ | NEQ | LT | GT | LE | GE) <concat>
<concat>     ::= <sum> | <sum> DPLUS <concat> 
<sum>        ::= <product> ((PLUS | MINUS) <product>)*
<product>    ::= <pow> ((STAR | SLASH) <pow>)* 
<pow>        ::= <prefix> | <prefix> DSTAR <pow> 
<prefix>     ::= (<prefix_op>)* <apply>
<prefix_op>  ::= MINUS | EXCL | LOG | EXP | SIN | COS | TAN | ASIN | ACOS | ATAN | HEAD |
                 TAIL | FST | SND 
<apply>      ::= <postfix> (APP <postfix>)* 
<postfix>    ::= <primary> (DPLUS | DMINUS | EXCL)* 
<primary>    ::= <ident> LARR <expr> | <literal> | LBR <expr> RBR 
<ident>      ::= <alpha_util> (<alnum_util>)* 
<literal>    ::= <int_lit> | <real_lit> | <char_lit> | <string_lit> | <tuple_lit> | 
                 <list_lit> | <dict_lit> | <fun_lit>
<int_lit>    ::= <num_util>
<real_lit>   ::= <real_util>
<char_lit>   ::= SQ <item_util> SQ
<string_lit> ::= DQ (<item_util>)* DQ
<tuple_lit>  ::= LB <expr> COM <expr> RB
<list_lit>   ::= LSB ((<expr> (COM <expr>)*) | eps) RSB
<dict_lit>   ::= LCB ((<dict_entry> (COM <dict_entry>)*) | eps) RCB
<dict_entry> ::= <expr> CLN <expr>
<fun_lit>    ::= BSLASH <ident> RARR <expr>


Utility parsers:

<num_util>   ::= (<digit>)+ 
<digit>      ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

<real_util>  ::= (MINUS | eps) <num_util> DOT <num_util> 

<item_util>  ::= ANY_CHAR 

<alpha_util> ::= LETTER 

<alnum_util> ::= <alpha_util> | <digit>

-}