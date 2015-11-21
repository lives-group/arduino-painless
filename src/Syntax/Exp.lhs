Expressions
===========

Definition of expressions for arduino programming.        
        
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE StandaloneDeriving #-}

> module Syntax.Exp where

> import Data.String
> import Data.Ratio
  
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
> instance IsIntegral Float
> instance IsIntegral Double    

> instance Integral Float where
>   toInteger = error "Prelude.Integral.toInteger applied to Float"
>   quotRem = error "Prelude.Integral.quotRem applied to Float"

> instance Integral Double where
>   toInteger = error "Prelude.Integral.toInteger applied to Double"
>   quotRem = error "Prelude.Integral.quotRem applied to Double"

      
types that support division

> class (IsIntegral t, Fractional t) => IsFractional t      
  
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

Some default instances derivation
    
> deriving instance Show (Exp t)

Test for atomic expressions
  
> atomic :: Exp t -> Bool
> atomic (EBool _)   = True
> atomic (EChar _)   = True
> atomic (EInt _)    = True
> atomic (EFloat _)  = True
> atomic (EDouble _) = True
> atomic (EString _) = True
> atomic _           = False
    
Numeric instances for Exp t

> class IsNum t => Constant t where
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
>     abs _         = error "Prelude.Num.abs applied to EDSL types"
>     signum _      = error "Prelude.Num.signum applied to EDSL types"           
>     fromInteger   = constant . fromInteger

> instance Enum (Exp t) where
>     fromEnum = error "Prelude.Enum.fromEnum applied to EDSL types"
>     toEnum = error "Prelude.Enum.toEnum applied to EDSL types"
      
> instance (Constant t, IsFractional t) => Integral (Exp t) where
>     div  = EDiv
>     mod  = EMod
>     quot = EDiv
>     rem  = error "Prelude.Integral.rem applied to EDSL types"

> instance (Constant t, IsFractional t, Show t) => Fractional (Exp t) where
>     (/) = EDiv
>     fromRational e = fromInteger (numerator e) / fromInteger (denominator e)
>     recip = error "Prelude.Fractional.recipe applied to EDSL types"

> instance Ord (Exp t)
> instance Eq (Exp t)
      
> instance (Constant t, Ord t) => Real (Exp t) where
>     toRational = error "Prelude.Real.toRational applied to EDSL types"
      
Overloaded String support

> instance (t ~ String) => IsString (Exp t) where
>     fromString = EString
