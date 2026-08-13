module ParserUtils where 


import Error 


import Data.Char 
import Control.Applicative 
import Data.Maybe 


-- A parser is a function from strings to things and strings, or an error... 
newtype Parser a = Parser {parse :: String -> Either Error (a, String)}


-- We must be able to apply pure unary function to a parser
instance Functor Parser where 
    -- fmap :: (a -> b) -> Parser a -> Parser b 
    fmap f px = Parser $ \s -> 
        (\(x, s') -> (f x, s')) <$> parse px s 


-- We must be able to apply pure any-ary function to parsers 
instance Applicative Parser where 
    -- pure :: a -> Parser a 
    pure x = Parser $ \s -> Right (x, s)

    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b 
    pf <*> px = Parser $ \s -> do 
        (f, s') <- parse pf s 
        (x, s'') <- parse px s' 
        pure (f x, s'')


-- We must be able to produce a new parser based on the internal value of the parser  
instance Monad Parser where 
    -- return :: a -> Parser a 
    return = pure 

    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b 
    mx >>= f = Parser $ \s -> do 
        (x, s') <- parse mx s 
        parse (f x) s'


-- We must be able to choose between two errros 
instance Alternative (Either Error) where 
    -- empty :: Either Error a
    empty = Left $ ParseError NoRuleMatchesError

    -- (<|>) :: Either Error a -> Either Error a -> Either Error a
    (Left _) <|> e = e 
    x@(Right e) <|> _ = x  


-- We must be able to choose between two parsers 
instance Alternative Parser where 
    -- empty :: Parser a 
    empty = Parser $ \s -> Left $ ParseError NoRuleMatchesError

    -- (<|>) :: Parser a -> Parser a -> Parser a 
    px <|> py = Parser $ \s ->
        parse px s <|> parse py s 


-- Now that the machinery is ready, we can start making basic parsers 


satisfy :: (Char -> Bool) -> Parser Char 
satisfy f = Parser $ \s -> case s of 
    []                 -> Left $ ParseError NothingToParseError 
    (c:cs) | f c       -> Right (c, cs) 
           | otherwise -> Left $ ParseError DoesNotSatisfyError 


item :: Parser Char 
item = satisfy (const True)


char :: Char -> Parser Char 
char = satisfy . (==) 


alpha :: Parser Char 
alpha = satisfy (isAlpha)


digit :: Parser Char 
digit = satisfy (isDigit)


alphanum :: Parser Char
alphanum = alpha <|> digit 


string :: String -> Parser String 
string = foldr (\c acc -> (:) <$> char c <*> acc) (pure [])


num :: Parser String 
num = some digit 


real :: Parser String 
real = (:) <$> char '-' <*> rest <|> rest 
  where
    rest :: Parser String 
    rest = (++) <$> num <*> ((:) <$> char '-' <*> num)