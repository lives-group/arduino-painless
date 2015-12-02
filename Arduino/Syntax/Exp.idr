module Arduino.Syntax.Exp

import Arduino.Basics.Base
import Arduino.Syntax.Ty

%default total

-- definition of expressions

data Exp : (env : Env) -> Type -> Type where
  -- variables
  EVar : member s env = Just t -> Exp env t
  -- constants
  EBool : Bool -> Exp env Bool
  EChar : Char -> Exp env Char
  EString : String -> Exp env String
  ENum : Num t => t -> Exp env t
  -- num bin ops
  EAdd  : Num t => Exp env t -> Exp env t -> Exp env t
  ESub  : Neg t => Exp env t -> Exp env t -> Exp env t
  EMult : Num t => Exp env t -> Exp env t -> Exp env t
  EDiv  : Integral t => Exp env t -> Exp env t -> Exp env t
  EMod  : Integral t => Exp env t -> Exp env t -> Exp env t
  -- boolean bin ops
  ENot : Exp env Bool -> Exp env Bool
  EAnd : Exp env Bool -> Exp env Bool -> Exp env Bool
  EOr  : Exp env Bool -> Exp env Bool -> Exp env Bool
  -- relational ops
  EEq  : Exp env t -> Exp env t -> Exp env Bool
  ENEq : Exp env t -> Exp env t -> Exp env Bool
  EGt  : Exp env t -> Exp env t -> Exp env Bool
  EGte : Exp env t -> Exp env t -> Exp env Bool
  ELt  : Exp env t -> Exp env t -> Exp env Bool
  ELte : Exp env t -> Exp env t -> Exp env Bool
        
instance Num t => Num (Exp env t) where
  (+) = EAdd
  (*) = EMult
  fromInteger = ENum . fromInteger
