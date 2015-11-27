module Test 

%default total

data Ty : Type where
  TInt  : Ty
  TBool : Ty
  
interpTy : Ty -> Type
interpTy TInt = Int
interpTy TBool = Bool  

boolNotInt : Not (TBool = TInt)
boolNotInt Refl impossible

instance DecEq Ty where
  decEq TInt TInt = Yes Refl
  decEq TBool TBool = Yes Refl
  decEq TInt TBool = No (boolNotInt . sym)
  decEq TBool TInt = No boolNotInt
  
data Env : Type where
  Nil  : Env
  Cons : String -> Ty -> Env -> Env
  
data TypeOf : String -> Ty -> Env -> Type where
  Here  : TypeOf s t (Cons s t env)
  There : TypeOf s t env -> TypeOf s t (Cons s' t' env)

noTypeNil : Not (TypeOf s t [])
noTypeNil Here impossible    

typeOf : Dec (TypeOf s t env)
typeOf {s = s}{t = t}{env = []} = No noTypeNil
typeOf {s = s}{t = t}{env = (Cons s' t' env)} with (decEq s s')
  typeOf {s = s}{t = t}{env = (Cons s' t' env)} | (Yes prf) with (decEq t t')
    typeOf {s = s}{t = t}{env = (Cons s t env)} | (Yes Refl) | (Yes Refl) = Yes Here
    typeOf {s = s}{t = t}{env = (Cons s' t' env)} | (Yes prf) | (No contra) 
           with (typeOf {s = s} {t = t} {env = env})
      typeOf {s = s}{t = t}{env = (Cons s' t' env)} | (Yes prf) | (No cpr) | (Yes pr) = Yes (There pr)
      typeOf {s = s}{t = t}{env = (Cons s' t' env)} | (Yes prf) | (No cpr) | (No f) = ?rhs_3
  typeOf {s = s}{t = t}{env = (Cons s' t' env)} | (No contra) = ?rhs_2
  
data Exp : Ty -> Type where
  EInt  : Int -> Exp TInt
  EBool : Bool -> Exp TBool
  EPlus : Exp TInt -> Exp TInt -> Exp TInt
  EAnd  : Exp TBool -> Exp TBool -> Exp TBool
  EIf   : Exp TBool -> Exp t -> Exp t  
  
data Stmt : Env -> Type where
  Nop    : Stmt env
  Decl   : (s : String) -> (t : Ty) -> Stmt (Cons s t env) -> Stmt env
  Assign : (s : String) -> (e : Exp t) -> (pr : TypeOf s t env) -> Stmt env -> Stmt env 


prog : Stmt Nil 
prog = Decl "x" TInt (Assign "x" (EInt 0) Here Nop)
