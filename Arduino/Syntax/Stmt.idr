module Arduino.Syntax.Stmt

import Arduino.Basics.Base
import Arduino.Syntax.Exp
import Arduino.Syntax.Ty

%default total

-- definition of statements

data Stmt : (env : Env) -> Type where
  Nop        : Stmt env
  VarDef     : (s : String) -> (t : Ty) -> Stmt ((s,t) :: env) -> Stmt env
  Assign     : (s : String) -> Exp env t -> Prop (member s t env) -> Stmt env
  While      : Exp env TBool -> Stmt env -> Stmt env
  IfThenElse : Exp env TBool -> Stmt env -> Stmt env -> Stmt env
