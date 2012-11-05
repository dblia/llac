(* Interface file for intermediate code generator *)

(* Semantics value struct:
  * This struct is used as a return value by expressions in the semantic
  * analysis of llamac project. The sem_val struct consists of the information
  * about the expression (value_type and expr_type) and about the intermediate
  * code to be generated (PLACE, NEXT, TRUE and FALSE attribute variables ). *)
type value_type = Lval | Rval | Dummy

type sem_val = {
  val_type  : value_type;
  expr_type : Types.ty;
  mutable place  : string;
  mutable next   : int list;
  mutable true_  : int list;
  mutable false_ : int list;
}
