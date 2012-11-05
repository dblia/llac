(* Intermediate code generator in quadruple form *)

open Types

(* Semantics value struct: *)
type value_type = Lval | Rval | Dummy

type sem_val = {
  val_type  : value_type;
  expr_type : Types.ty;
  mutable place  : string;
  mutable next   : int list;
  mutable true_  : int list;
  mutable false_ : int list;
}

