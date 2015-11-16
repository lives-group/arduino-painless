Expressions
===========

Definition of expressions for arduino programming.        
        
> {-# LANGUAGE GADTs #-}
> module Syntax.Exp where

Expression syntax
-----------------

> data Exp t where
>   EConst :: (IsScalar t) => t -> Exp t
                     
