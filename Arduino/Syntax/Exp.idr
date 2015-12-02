module Arduino.Syntax.Exp

import Arduino.Basics.Base
import Arduino.Syntax.Ty

%default total

-- definition of expressions

data Exp : (env : Env) -> (t : Ty) -> Type where
  -- variables
  EVar : Prop (member s t env) -> Exp env t
  -- constants
  EBool : Bool -> Exp env TBool
  EChar : Char -> Exp env TChar
  EString : String -> Exp env TString
  ENum : Num (typeOf t) => (typeOf t) -> Exp env t
  -- num bin ops
  EAdd  : Num (typeOf t) => Exp env t -> Exp env t -> Exp env t
  ESub  : Neg (typeOf t) => Exp env t -> Exp env t -> Exp env t
  EMult : Num (typeOf t) => Exp env t -> Exp env t -> Exp env t
  EDiv  : Integral (typeOf t) => Exp env t -> Exp env t -> Exp env t
  EMod  : Integral (typeOf t) => Exp env t -> Exp env t -> Exp env t
  -- boolean bin ops
  ENot : Exp env TBool -> Exp env TBool
  EAnd : Exp env TBool -> Exp env TBool -> Exp env TBool
  EOr  : Exp env TBool -> Exp env TBool -> Exp env TBool
  -- relational ops
  EEq  : Exp env t -> Exp env t -> Exp env TBool
  ENEq : Exp env t -> Exp env t -> Exp env TBool
  EGt  : Exp env t -> Exp env t -> Exp env TBool
  EGte : Exp env t -> Exp env t -> Exp env TBool
  ELt  : Exp env t -> Exp env t -> Exp env TBool
  ELte : Exp env t -> Exp env t -> Exp env TBool

instance Cast Integer (typeOf t) where
  cast {t = TBool} = \ i => i > 0
  cast {t = TInt} = id
  cast {t = TChar} = cast . the Int
  cast {t = TFloat} = cast
  cast {t = TDouble} = cast
  cast {t = TString} = cast
        

instance Num (typeOf t) => Num (Exp env t) where
  (+) = EAdd
  (*) = EMult
  fromInteger = ENum . cast
