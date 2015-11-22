Expression evaluation
=====================

Simple evaluation semantics 

> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE StandaloneDeriving #-}
> {-# LANGUAGE TypeFamilies #-}


> module Eval.Eval where  

> import Control.Monad  
> import Control.Monad.Identity  

> import Data.String
  
> import Syntax.Exp
  
Definition of evaluation monad
  
> type EvalM a = Identity a

Evaluation logic typeclass     

> class Eval a where
>    type Result a
>    eval :: a -> EvalM (Result a)


> instance Eval (Exp t) where
>    type Result (Exp t) = t

>    eval (EBool b)    = return b
>    eval (EChar c)    = return c
>    eval (EInt i)     = return i
>    eval (EFloat f)   = return f                 
>    eval (EDouble d)  = return d
>    eval (EString s)  = return s
>    eval (EAdd e e')  = liftM2 (+) (eval e) (eval e')
>    eval (ESub e e')  = liftM2 (-) (eval e) (eval e')
>    eval (EMult e e') = liftM2 (*) (eval e) (eval e')
>    eval (EDiv e e')  = liftM2 (/) (eval e) (eval e')
>    eval (EMod e e')  = liftM2 mod (eval e) (eval e')
>    eval (EAnd e e')  = liftM2 (&&) (eval e) (eval e')
>    eval (EOr e e')   = liftM2 (||) (eval e) (eval e')
>    eval (ENot e)     = liftM not (eval e)
>    eval (EEq e e')   = liftM2 (==) (eval e) (eval e')
>    eval (ENEq e e')  = liftM2 (/=) (eval e) (eval e')
>    eval (EGeq e e')  = liftM2 (>=) (eval e) (eval e')
>    eval (EGt e e')   = liftM2 (>) (eval e) (eval e')
>    eval (ELeq e e')  = liftM2 (<=) (eval e) (eval e')
>    eval (ELt e e')   = liftM2 (<) (eval e) (eval e')
