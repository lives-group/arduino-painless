Statements for C like language
==============================

> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE ExplicitForAll #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}

> module Syntax.Stmt where  

> import Syntax.Exp
  
definition of statement syntax

> data Env = Nil | forall a . Cons String a Env

> data In (s :: String) (env :: Env) where
>     Here  :: forall a . In s (Cons s a env)
>     There :: forall a b. In s env -> In s (Cons s a env)       

  
Better syntax for assignment
  

