module Tests.SyntaxRules

import Arduino.Basics.Base

import Arduino.Syntax.Exp
import Arduino.Syntax.Stmt
import Arduino.Syntax.SyntaxDef
import Arduino.Syntax.Ty

-- some boring tests

prog0 : Stmt []
prog0 =  VarDef "y" Int
             (VarDef "x" Int
                 (Assign "x" (EAdd (EVar "y" Refl)
                                   (ENum 0))
                              Refl
                              Nop))


prog1 : Stmt []
prog1 = begin
          int "y" ; 
          int "x" ; 
          "x" := (use "y") + 0 ; 
        end

{-                
        
prog2 : Stmt []
prog2 = begin
          int "n"  ;
          int "r"  ;
          "r" := 1 ;
          while ((use "n") :>= 0) {
            "r" := (use "r") * (use "n") ;
            "n" := (use "n") - 1 ; 
          }          
        end
-}
