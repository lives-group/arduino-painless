Expressions
===========

Definition of expressions for arduino programming.        
        
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE OverloadedStrings #-}

> module Syntax.Exp where

> import Data.String
  
Expression syntax
-----------------

types that support basic numeric operations
        
> class Num t => IsNum t

> instance IsNum Int
> instance IsNum Float
> instance IsNum Double

types that support equality  
  
> class Eq t => IsEq t

> instance IsEq Bool
> instance IsEq Char
> instance IsEq Int
> instance IsEq Float
> instance IsEq Double
> instance IsEq String

types that support modulus  
  
> class (IsNum t, Integral t) => IsIntegral t

> instance IsIntegral Int

types that support division

> class (IsNum t, Fractional t) => IsFractional t      

> instance IsFractional Float
> instance IsFractional Double

types that support comparison

> class (IsEq t, Ord t) => IsOrd t

> instance IsOrd Bool
> instance IsOrd Char
> instance IsOrd Int
> instance IsOrd Float
> instance IsOrd Double
> instance IsOrd String
  
> data Exp t where
>   EBool   :: Bool -> Exp Bool
>   EChar   :: Char -> Exp Char
>   EInt    :: Int  -> Exp Int
>   EFloat  :: Float -> Exp Float
>   EDouble :: Double -> Exp Double
>   EString :: String -> Exp String
>   EAdd    :: (IsNum t) => Exp t -> Exp t -> Exp t
>   EMult   :: (IsNum t) => Exp t -> Exp t -> Exp t
>   ESub    :: (IsNum t) => Exp t -> Exp t -> Exp t
>   EDiv    :: (IsFractional t) => Exp t -> Exp t -> Exp t
>   EMod    :: (IsIntegral t)   => Exp t -> Exp t -> Exp t
>   EAnd    :: Exp Bool -> Exp Bool -> Exp Bool
>   EOr     :: Exp Bool -> Exp Bool -> Exp Bool
>   ENot    :: Exp Bool -> Exp Bool
>   EEq     :: (IsEq t) => Exp t -> Exp t -> Exp Bool
>   ENEq    :: (IsEq t) => Exp t -> Exp t -> Exp Bool
>   EGeq    :: (IsOrd t) => Exp t -> Exp t -> Exp Bool
>   EGt     :: (IsOrd t) => Exp t -> Exp t -> Exp Bool
>   ELeq    :: (IsOrd t) => Exp t -> Exp t -> Exp Bool
>   ELt     :: (IsOrd t) => Exp t -> Exp t -> Exp Bool

Numeric instances for Exp t

> class (IsNum t) => Constant t where
>    constant :: t -> Exp t

> instance Constant Int where
>    constant = EInt

> instance Constant Float where
>    constant = EFloat

> instance Constant Double where
>    constant = EDouble
     
> instance Constant t => Num (Exp t) where
>     (+)           = EAdd
>     (-)           = ESub
>     (*)           = EMult
>     negate e      = 0 - e
>     abs _         = error "Prelude.Num.abs applied to EDSL type"
>     signum _      = error "Prelude.Num.signum applied to EDSL type"           
>     fromInteger   = constant . fromInteger

Overloaded String support

> instance (t ~ String) => IsString (Exp t) where
>     fromString = EString
