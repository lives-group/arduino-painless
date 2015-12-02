module Arduino.Syntax.Stmt

import Arduino.Basics.Base
import Arduino.Syntax.Exp
import Arduino.Syntax.Ty

%default total

-- definition of statements

data Stmt : (env : Env) -> Type where
  Nop        : Stmt env
  VarDef     : (s : String) -> (t : Type) -> Stmt ((s,t) :: env) -> Stmt env
  Assign     : (s : String) -> Exp env t -> member s env = Just t -> Stmt env -> Stmt env
  While      : Exp env Bool -> Stmt env -> Stmt env -> Stmt env
  IfThenElse : Exp env Bool -> Stmt env -> Stmt env -> Stmt env
