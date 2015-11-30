module Test 

%default total

data Ty : Type where
  TInt  : Ty
  TBool : Ty
  
interpTy : Ty -> Type
interpTy TInt = Int
interpTy TBool = Bool  

instance Eq Ty where
  TInt == TInt = True
  TBool == TBool = True
  _ == _ = False
  
Env : Type
Env = List (String , Ty)

member : String -> Ty -> Env -> Bool
member s t [] = False
member s t ((s',t') :: env) = ((s == s') && (t == t')) || member s t env
  
Prop : Bool -> Type
Prop True = Unit
Prop False = Void  
  
data Exp : (e : Env) -> (t : Ty) -> Type where
  EVar  : (s : String) -> {t : Ty} -> Prop (member s t e) -> Exp e t
  EInt  : Int -> Exp e TInt
  EBool : Bool -> Exp e TBool
  EPlus : Exp e TInt -> Exp e TInt -> Exp e TInt
  EAnd  : Exp e TBool -> Exp e TBool -> Exp e TBool
  EIf   : Exp e TBool -> Exp e t -> Exp e t  
  
data Stmt : Env -> Type where
  Nop    : Stmt env
  Decl   : (s : String) -> (t : Ty) -> Stmt ((s , t) :: env) -> Stmt env
  Assign : (s : String) -> (e : Exp env t) -> 
           (k : Prop (member s t env)) -> 
           Stmt env -> Stmt env 

prog : Stmt Nil 
prog = Decl "x" TInt (Assign "x" (EInt 0) () Nop)

