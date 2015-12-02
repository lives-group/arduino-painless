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
syntax false = EBool false
syntax use [var] = EVar {s = var} () 
syntax [e] ":+" [e'] = EAdd Refl e e'
 
-- statements

syntax [var] ":=" [exp] = Assign var exp ()

