module ExprParser where 


import ParseUtils 
import Expr 
import Stmt 
import Control.Applicative
import qualified Data.Map as Map
import Language.Haskell.TH.Lib (typeP)

-- BNF 

{-

<program>    ::= (<stmt> SEMI)*
<stmt>       ::= (<declare> | <assign> | <print>) 

<declare>    ::= <ident> AT <type> 
<assign>     ::= <ident> LARR <expr>
<print>      ::= PRINT <expr> 
<type>       ::= <type_atom> <type_arrow>
<type_atom>  ::= INT | REAL | CHAR | STRING | LSB <type> RSB | LB <type> COM <type> RB |
                 LCB <type> DRARR <type> RCB
<type_arrow> ::= RARR <type> | eps 

<expr>       ::= <if_expr> | <cons> 
<if_expr>    ::= IF <expr> QM <expr> CLN <expr>
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

<float_util> ::= <num_util> DOT <num_util> 

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


-- Parser for binary left-associative expression. (BinOp, String) pairs represent BinOp and corresponding string 
binL :: [(BinOp, String)] -> Parser Expr -> Parser Expr
binL ops p = foldl (\e (op, e') -> BinOp op e e') <$> p <*> many binL' 
  where
    binL' :: Parser (BinOp, Expr)
    binL' = (,) <$> (foldr (\(bop, s) px -> bop <$ string s <|> px) empty ops) <*> p 


binR :: [(BinOp, String)] -> Parser Expr -> Parser Expr
binR ops p = p <**> (foldr step id <$> many binR')
  where
    step :: (BinOp, Expr) -> (Expr -> Expr) -> Expr -> Expr 
    step (op, e) f x = BinOp op x (f e) 

    binR' :: Parser (BinOp, Expr)
    binR' = (,) <$> (foldr (\(bop, s) px -> bop <$ string s <|> px) empty ops) <*> p 


ident :: Parser String 
ident = (:) <$> alpha <*> many alphanum 


int :: Parser Lit 
int = LInt . read <$> num 


real :: Parser Lit 
real = LReal . read <$> float 


charLit :: Parser Lit 
charLit = LChar <$> (char '\'' *> item <* char '\'') 


stringLit :: Parser Lit 
stringLit = LString <$> many (char '\"' *> item <* char '\"') 


pair :: Parser Lit 
pair = (curry LPair) <$> (char '(' *> expr <* char ',') <*> (expr <* char ')')


list :: Parser Lit
list = 
  LList <$> (char '[' *> (((:) <$> expr <*> (char ',' *> many expr)) <|> pure []) <* char ']')


dict :: Parser Lit 
dict = 
  LDictionary . Map.fromList <$> (char '{' *> (((:) <$> entry <*> (char ',' *> many entry)) <|> pure []) <* char '}')
    where
      entry :: Parser (Expr, Expr)
      entry = (,) <$> expr <*> (string "=>" *> expr)


fun :: Parser Lit
fun = LFun <$> (char '\\' *> ident) <*> (string "->" *> expr) 


lit :: Parser Expr 
lit = Lit <$> (int <|> real <|> charLit <|> stringLit <|> pair <|> list <|> dict <|> fun)


primary :: Parser Expr 
primary = Var <$> ident <|> lit <|> char '(' *> expr <* char ')'


postfix :: Parser Expr 
postfix = foldl (flip UnOp) <$> primary <*> many op 
  where
    op :: Parser UnOp 
    op = Inc <$ string "++" <|> Dec <$ string "--" <|> Fact <$ char '!'


apply :: Parser Expr 
apply = binL [(Apply, "$")] postfix 


prefix :: Parser Expr 
prefix = (flip $ foldr UnOp) <$> (many op) <*> apply 
  where
    op :: Parser UnOp
    op = 
      Not <$ char '!' <|> 
      Log <$ string "log" <|> 
      Exp <$ string "exp" <|> 
      Sin <$ string "sin" <|>  
      Cos <$ string "cos" <|> 
      Tan <$ string "tan" <|> 
      ASin <$ string "asin" <|> 
      ACos <$ string "acos" <|> 
      ATan <$ string "atan" 

    
pow :: Parser Expr 
pow = binR [(Pow, "**")] prefix 


proddiv :: Parser Expr 
proddiv = binL [(Mul, "*"), (Div, "/")] pow 


sumdiff :: Parser Expr 
sumdiff = binL [(Add, "+"), (Sub, "-")] proddiv 


concatBin :: Parser Expr 
concatBin = binR [(Concat, "++")] sumdiff


comp :: Parser Expr 
comp = flip BinOp <$> concatBin
  <*> (Eq <$ string "=" <|> 
       NEq <$ string "/=" <|> 
       LE <$ string "<=" <|> 
       Expr.LT <$ string "<" <|>
       GE <$ string ">=" <|>
       Expr.GT <$ string ">") 
  <*> concatBin 
  <|> concatBin 


andBin :: Parser Expr 
andBin = binL [(And, "&&")] comp 


xorBin :: Parser Expr 
xorBin = binL [(Xor, "^^")] andBin


orBin :: Parser Expr 
orBin = binL [(Or, "||")] xorBin 


consOp :: Parser Expr 
consOp = binR [(Cons, "::")] orBin 


ternary :: Parser Expr 
ternary = If <$> (string "if" *> expr) <*> (string "?" *> expr) <*> (string ":" *> expr)


expr :: Parser Expr
expr = ternary <|> consOp 


typeAtom :: Parser Type 
typeAtom 
  =   Int <$ string "Int" 
  <|> Real <$ string "Real"
  <|> Char <$ string "Char"
  <|> String <$ string "String"
  <|> List <$> (char '[' *> typeAtom <* char ']')
  <|> Pair <$> (char '(' *> typeAtom <* char ',') <*> (typeAtom <* char ')')
  <|> Dictionary <$> (char '{' *> typeAtom <* string "=>") <*> (typeAtom <* char '}') 


typeParser :: Parser Type 
typeParser =  flip ($) <$> typeAtom <*> typeArr 
  where
    typeArr :: Parser (Type -> Type)
    typeArr = (flip Fun) <$> (string "->" *> typeParser) <|> pure id 


printParser :: Parser Stmt 
printParser = string "print" *> (Print <$> expr)


declare :: Parser Stmt 
declare = Declare <$> ident <*> (char '@' *> typeParser)


assign :: Parser Stmt 
assign = Assign <$> ident <*> (string "<-" *> expr)
 

stmt :: Parser Stmt 
stmt = printParser <|> declare <|> assign


program :: Parser [Stmt]
program = many (stmt <* char ';')