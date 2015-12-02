module Arduino.Syntax.Ty

%default total

-- definition of types

data Ty : Type where
  TBool   : Ty
  TChar   : Ty
  TString : Ty
  TInt    : Ty
  TFloat  : Ty
  TDouble : Ty
  
  
-- type interpretation

typeOf : Ty -> Type
typeOf TBool = Bool
typeOf TChar = Char
typeOf TString = String
typeOf TInt = Integer
typeOf TFloat = Float
typeOf TDouble = Double    
  
  
-- equality for types

instance Eq Ty where
  TBool == TBool = True
  TChar == TChar = True
  TInt == TInt = True
  TFloat == TFloat = True
  TDouble == TDouble = True
  TString == TString = True
  _ == _ = False
  
  
-- environments

Env : Type
Env = List (String , Ty)

member : String -> Ty -> Env -> Bool
member s t [] = False
member s t ((s',t') :: env) = ((s == s') && (t == t')) || member s t env
 
