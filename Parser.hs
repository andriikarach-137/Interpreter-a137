module Parser where 


import ParseUtils 


-- BNF 

{-

<program>    ::= (<stmt> SEMI)*
<stmt>       ::= (<assign> | <print>) 

<assign>     ::= <type> <ident> LARR <expr>
<print>      ::= PRINT <expr> 
<type>       ::= <type_atom> <type_arrow>
<type_atom>  ::= INT | REAL | CHAR | STRING | LSB <type> RSB | LB <type> COM <type> RB |
                 LCB <type> DRARR <type> RCB
<type_arrow> ::= RARR <type> | eps 

<expr>       ::= <cons> <ternary> 
<ternary>    ::= QM <expr> CLN <expr> | eps 
<cons>       ::= <or> | <or> DCLN <cons> 
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
<primary>    ::= <ident> | <literal> | LBR <expr> RBR 
<ident>      ::= <alpha_util> (<alnum_util>)* 
<literal>    ::= <int_lit> | <real_lit> | <char_lit> | <string_lit> | <pair_lit> | 
                 <list_lit> | <dict_lit> | <fun_lit>
<int_lit>    ::= <num_util>
<real_lit>   ::= <real_util>
<char_lit>   ::= SQ <item_util> SQ
<string_lit> ::= DQ (<item_util>)* DQ
<pair_lit>   ::= LB <expr> COM <expr> RB
<list_lit>   ::= LSB ((<expr> (COM <expr>)*) | eps) RSB
<dict_lit>   ::= LCB ((<dict_entry> (COM <dict_entry>)*) | eps) RCB
<dict_entry> ::= <expr> DRARR <expr>
<fun_lit>    ::= BSLASH <ident> RARR <expr>


Utility parsers: @f denotes utility parser constructed as satisfy f 

<num_util>   ::= (<digit>)+
<digit>      ::= @isDigit 

<real_util>  ::= <num_util> DOT <num_util> 

<item_util>  ::= @(\c -> c /= '\'' && c /= '\"')

<alpha_util> ::= @(isAlpha) 

<alnum_util> ::= @(isAlphaNum)


Terminals:

SEMI         ::= ";"
LARR         ::= "<-"
PRINT        ::= "print"
CLN          ::= ":"
OR           ::= "||"
XOR          ::= "^^"
AND          ::= "&&"
EQ           ::= "="
NEQ          ::= "/="
LT           ::= "<"
GT           ::= ">"
LE           ::= "<="
GE           ::= ">="
DPLUS        ::= "++"
PLUS         ::= "+"
MINUS        ::= "-"
STAR         ::= "*"
SLASH        ::= "/"
DSTAR        ::= "**"
EXCL         ::= "!"
LOG          ::= "log"
EXP          ::= "exp"
SIN          ::= "sin"
COS          ::= "cos"
TAN          ::= "tan"
ASIN         ::= "asin"
ACOS         ::= "acos"
ATAN         ::= "atan"
HEAD         ::= "head"
TAIL         ::= "tail"
FST          ::= "fst"
SND          ::= "snd"
APP          ::= "$"
DMINUS       ::= "--"
RARR         ::= "->"
LBR          ::= "("
RBR          ::= ")"
SQ           ::= "'"
DQ           ::= "\""
LSB          ::= "["
RSB          ::= "]"
LCB          ::= "{"
RCB          ::= "}"
COM          ::= ","
BSLASH       ::= "\"
DRARR        ::= "=>"
QM           ::= "?"
DCLN         ::= "::"
-}