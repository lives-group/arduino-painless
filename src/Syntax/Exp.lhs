Expressions
===========

Definition of expressions for arduino programming.        
        
> {-# LANGUAGE GADTs #-}
> module Syntax.Exp where

> import Data.Byte
> import Data.Word
  
Expression syntax
-----------------

types that support basic numeric operations
        
> class IsNum t

> instance IsNum Int
> instance IsNum Float
> instance IsNum Double     

> data Exp t where
>   EBool   : Bool -> Exp Bool
>   EChar   : Char -> Exp Char
>   EByte   : Byte -> Exp Byte
>   EInt    : Int  -> Exp Int
>   EWord   : Word -> Exp Word
>   EFloat  : Float -> Exp Float
>   EDouble : Double -> Exp Double
>   EString : String -> Exp String
>   EArray  : [Exp t] -> Exp t
>   EAdd    : (IsNum t) => Exp t -> Exp t -> Exp t
>   EMult   : (IsNum t) => Exp t -> Exp t -> Exp t
>   ESub    : (IsNum t) => Exp t -> Exp t -> Exp t
>   EDiv    : (IsNum t) => Exp t -> Exp t -> Exp t
>   EMod    : (IsNum t) => Exp t -> Exp t -> Exp t
>   EAnd    : Exp Bool -> Exp Bool -> Exp Bool
>   EOr     : Exp Bool -> Exp Bool -> Exp Bool
>   ENot    : Exp Bool -> Exp Bool
>   EEq     : (IsEq t) => Exp t -> Exp t -> Exp Bool
>   ENEq    : (IsEq t) => Exp t -> Exp t -> Exp Bool
>   EGeq    : (IsEq t) => Exp t -> Exp t -> Exp Bool
>   EGt     : (IsEq t) => Exp t -> Exp t -> Exp Bool
>   ELeq    : (IsEq t) => Exp t -> Exp t -> Exp Bool
>   ELt     : (IsEq t) => Exp t -> Exp t -> Exp Bool
>                                  
