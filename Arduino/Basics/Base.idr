module Arduino.Basics.Base

%default total

-- definition of propositions

Prop : Bool -> Type
Prop True  = Unit
Prop False = Void

