module Arduino.Syntax.SyntaxDef

import Arduino.Syntax.Exp
import Arduino.Syntax.Stmt
import Arduino.Syntax.Ty

%default total

-------------------------------------------------
-- some syntax rules for easy program writing  --
-------------------------------------------------

-- expressions

syntax true = EBool True
syntax false = EBool False
syntax use [var] = EVar {s = var} Refl
syntax [e] ":+" [e'] = EAdd e e'
syntax [e] ":>=" [e'] = EGte e e'
 
-- statements

syntax begin [s] = s
syntax end = Nop
syntax int [var] ";" [s] = VarDef var Int s
syntax [var] ":=" [exp] ";" [st] = Assign var exp Refl st
syntax [var] ":=" [exp] "." = Assign var exp Refl Nop
syntax while [exp] "{" [stmt] "}" [stmt'] = While exp stmt stmt'

