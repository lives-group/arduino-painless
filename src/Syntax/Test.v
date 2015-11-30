Set Implicit Arguments.

Require Import String List.

Inductive Ty : Set := TInt | TBool.

Definition eqTyDec (t t' : Ty) : {t = t'} + {t <> t'}.
  decide equality.
Defined.

Definition interpTy (t : Ty) : Set :=
  match t with
    | TInt  => nat
    | TBool => bool
  end.

Definition Entry : Set := (string * Ty)%type.

Definition entryEqDec : forall (e e' : Entry), {e = e'} + {e <> e'}.
  pose eqTyDec.
  pose string_dec.
  decide equality.
Defined.         

Definition Env : Set := list Entry.

Inductive Exp : Env -> Ty -> Set :=
  | ENum : forall {e}, nat -> Exp e TInt
  | EBool : forall {e}, bool -> Exp e TBool
  | EVar  : forall {s e t}, In (s,t) e -> Exp e t.

Inductive Stmt : Env -> Set :=
  | Nop : forall {e}, Stmt e
  | Def : forall {e} s t, Stmt ((s,t) :: e) -> Stmt e
  | Ass : forall {env t} s (e : Exp env t), In (s,t) env -> Stmt env.

Definition assign {env : Env}{t : Ty}(s : string)(e : Exp env t) : Stmt env.
  eapply (Ass s).
  exact e ;
  destruct (in_dec entryEqDec (s,t) env) ;
  match goal with
    | [H : In ?s ?t |- In s t] => auto
    | [H : ~ In ?s ?t |- In s t] => fail "undefined variable"                                 
  end.