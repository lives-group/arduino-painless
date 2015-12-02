module Arduino.Basics.Base

%default total

-- definition of propositions

Prop : Maybe a -> Type
Prop (Just _) = Unit
Prop Nothing  = Void

