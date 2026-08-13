module Error where 


-- Represents what types of errors can happen while parsing and running our program 
data Error = ParseError ParseError | RuntimeError 


data ParseError 
    = NoRuleMatchesError 
    | NothingToParseError 
    | DoesNotSatisfyError 