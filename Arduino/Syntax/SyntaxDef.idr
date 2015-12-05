module Arduino.Syntax.SyntaxDef

import Arduino.Syntax.Exp
import Arduino.Syntax.Stmt
import Arduino.Syntax.Ty

%default total

-------------------------------------------------
-- some syntax rules for easy program writing  --
-------------------------------------------------

-- expressions

true : Exp env Bool
true = EBool True

false : Exp env Bool
false = EBool Fasle

var : (s : String) -> Exp env t
var s = EVar s Refl

(:>=) : Exp env t -> Exp env t -> Exp env Bool
e :>= e' = EGte e e'
  
-- statements

(:;:) : Stmt env -> Smt (env ++ env') -> Stmt (env ++ env')
s :;: s' = Seq s s'

