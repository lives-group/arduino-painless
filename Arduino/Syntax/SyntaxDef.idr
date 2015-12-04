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
syntax use [var] = EVar var Refl
syntax [e] ":+" [e'] = EAdd e e'
syntax [e] ":>=" [e'] = EGte e e'
 
-- statements

syntax begin [s] = s
syntax end = Nop
syntax "int" [var] ";" [smt] = VarDef var Int smt
syntax [var] ":=" [exp] ";" [smt] = Assign var exp Refl smt
syntax while [exp] "{" [stmt] "}" smt = While exp (stmt Nop) smt


