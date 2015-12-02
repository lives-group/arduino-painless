module Tests.SyntaxRules

import Arduino.Basics.Base

import Arduino.Syntax.Exp
import Arduino.Syntax.Stmt
import Arduino.Syntax.SyntaxDef
import Arduino.Syntax.Ty

-- some boring tests

prog1 : Stmt []
prog1 = begin
           int "y" ; 
           int "x" ; 
           "x" := (use "y") + 0 ; 
        end

