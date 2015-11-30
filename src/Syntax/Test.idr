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
  
Env : Type
Env = List (String , Ty)

data In : {A : Type} -> A -> List A -> Type where
  Here  : In x (x :: xs)
  There : In x ys -> In x (y :: ys)  
  
InEnv : String -> Ty -> Env -> Type
InEnv s t env = In (s , t) env

inEnvInv : InEnv s t ((s',t') :: env) -> Either (s = s' , t = t') (InEnv s t env)
inEnvInv Here = Left (Refl , Refl)
inEnvInv (There pr) = Right pr

inEnvDec : Dec (InEnv s t env)
inEnvDec {s = s}{t = t}{env = []} = No (\ Here impossible)
inEnvDec {s = s}{t = t}{env = ((s',t') :: xs)} with (decEq s s')
  inEnvDec {s = s}{t = t}{env = ((s,t') :: xs)} | (Yes Refl) with (decEq t t')
    inEnvDec {s = s}{t = t}{env = ((s,t) :: xs)} | (Yes Refl) | (Yes Refl) = Yes Here
    inEnvDec {s = s}{t = t}{env = ((s,t') :: xs)} | (Yes Refl) | (No contra) 
      with (inEnvDec {s = s}{t = t} {env = xs})
      inEnvDec {s = s}{t = t}{env = ((s,t') :: xs)} | (Yes Refl) | (No ctr) | (Yes pr') 
               = Yes (There pr')
      inEnvDec {s = s}{t = t}{env = ((s,t') :: xs)} | (Yes Refl) | (No ctr) | (No ctr') 
               = No (either (ctr . snd) ctr' . inEnvInv)
  inEnvDec {s = s}{t = t}{env = ((s',t') :: xs)} | (No ctr) 
      with (inEnvDec {s = s}{t = t}{env = xs})
    inEnvDec {s = s}{t = t}{env = ((s',t') :: xs)} | (No ctr) | (Yes prf) = Yes (There prf)
    inEnvDec {s = s}{t = t}{env = ((s',t') :: xs)} | (No ctr) | (No ctr') 
               = No (either (ctr . fst) ctr' . inEnvInv)
  
data Exp : (e : Env) -> (t : Ty) -> Type where
  EVar  : (s : String) -> {t : Ty} -> InEnv s t e -> Exp e t
  EInt  : Int -> Exp e TInt
  EBool : Bool -> Exp e TBool
  EPlus : Exp e TInt -> Exp e TInt -> Exp e TInt
  EAnd  : Exp e TBool -> Exp e TBool -> Exp e TBool
  EIf   : Exp e TBool -> Exp e t -> Exp e t  
  
Prop : Bool -> Type
Prop True = Unit
Prop False = Void

dec : Dec a -> Bool
dec (Yes _) = True
dec (No _)  = False

data Stmt : Env -> Type where
  Nop    : Stmt env
  Decl   : (s : String) -> (t : Ty) -> Stmt ((s , t) :: env) -> Stmt env
  Assign : (s : String) -> (e : Exp env t) -> 
           (k : Prop (dec (inEnvDec {s = s}{t = t}{env = env}))) -> 
           Stmt env -> Stmt env 

prog : Stmt Nil 
prog = Decl "x" TInt (Assign "x" (EInt 0) ?rhs1 Nop)

