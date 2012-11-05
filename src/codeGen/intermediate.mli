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

(* New datatype definitions for quadruples handling *)
(* Operators *)
type operator = Empty

(* Operands *)
type operand = Empty

(* quad type definition *)
type quad = {
  label : int;
  mutable operator : operator;
  operand1         : operator;
  operand2         : operator;
  mutable operand3 : operator; (* for backpatching *)
}

(* Auxilary subroutines for quadruples handling *)
(* Returns the number of the next quad *)
val nextQuad : unit -> unit ;;

(* Generates new quad and increases quadNext number *)
val genQuad : operator -> operand -> operand -> operand -> quad ;;

(* Quad storage and handling functions *)
(* List of quadruples *)
val quad_list : quad list ref ;;

(* Get the quads (list needs reversal because of the way we store the quads) *)
val get_quads : unit -> quad list ;;

(* Add a quad to a list *)
val add_quad : quad -> unit ;;

(* Backpatching:
 * Replace all 'backpatch' operands in quads with labels in l to z *)
val backpatch : int list -> int -> unit ;;
