Pretty-Printting Support
=======================

> {-# LANGUAGE GADTs #-}
        
> module Pretty.PPrint where

> import Text.PrettyPrint.HughesPJ       

> import Syntax.Exp
  
A type class for pretty-printing stuff

> class PPrint t where
>     pprint :: t -> Doc

Expression pretty-printer

> parensIf :: PPrint t => Bool -> t -> Doc
> parensIf True  e = parens (pprint e)
> parensIf False e = pprint e                  

> infixOp :: Exp t -> Exp t -> String -> Doc
> infixOp e e' op = parensIf (atomic e) e <+> text op <+> pprint e'
           
> instance PPrint (Exp t) where
>    pprint (EBool b)    = text (show b)
>    pprint (EChar c)    = char c
>    pprint (EInt i)     = int i
>    pprint (EFloat f)   = float f
>    pprint (EDouble d)  = double d
>    pprint (EAdd e e')  = infixOp e e' "+"
>    pprint (ESub e e')  = infixOp e e' "-"
>    pprint (EMult e e') = infixOp e e' "*"
>    pprint (EDiv e e')  = infixOp e e' "/"
>    pprint (EMod e e')  = infixOp e e' "%"
>    pprint (EAnd e e')  = infixOp e e' "&&"
>    pprint (EOr e e')   = infixOp e e' "||"
>    pprint (ENot e)     = text "!" <+> pprint e
>    pprint (EEq e e')   = infixOp e e' "=="
>    pprint (ENEq e e')  = infixOp e e' "!="
>    pprint (EGeq e e')  = infixOp e e' ">="
>    pprint (EGt e e')   = infixOp e e' ">"
>    pprint (ELeq e e')  = infixOp e e' "<="
>    pprint (ELt e e')   = infixOp e e' "<"                      
