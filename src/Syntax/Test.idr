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
  
data Stmt : (env : Env) -> Type where
  Nop    : Stmt env
  Decl   : (s : String) -> (t : Ty) -> Stmt ((s , t) :: env) -> Stmt env
  Assign : (s : String) -> (e : Exp env t) -> 
           (k : Prop (member s t env)) -> 
           Stmt env -> Stmt env 

-- program without any sugar

prog : Stmt []
prog = Decl "y" TInt (
         Decl "x" TInt (
           Assign "x" (
             EPlus (EVar "y" ()) (EInt 0)
           )
           ()
           Nop
         )
       )

syntax [var] ":=" [exp] ";" [smt] = Assign var exp () smt
syntax int [var] ";" [stmt] = Decl var TInt stmt
syntax use [var] = EVar var ()
syntax [exp] ":+" [exp1] = EPlus exp exp1
syntax end = Nop

prog1 : Stmt []
prog1 = int "y" ; 
        int "x" ; 
        "x" := (use "y") :+ (EInt 0) ; 
        end
  
 
 
