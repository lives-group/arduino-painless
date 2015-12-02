module Arduino.Syntax.Ty

%default total

-- environments

Env : Type
Env = List (String , Type)

member : String -> Env -> Maybe Type
member s [] = Nothing
member s ((s',t') :: env) = if s == s' then Just t' else member s env
 
