Statements for C like language
==============================

> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE ExplicitForAll #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}

> module Syntax.Stmt where  

> import Syntax.Exp
> import GHC.TypeLits
  
definition of statement syntax

> data Env = Nil | forall a . Cons String a Env

> data In (s :: Symbol) (env :: Env) where
>     Here  :: forall a . In s (Cons s a env)
>     There :: forall a b. In s env -> In s (Cons s a env)       


data Stmt (env :: Env) where
     
        
Better syntax for assignment
  

