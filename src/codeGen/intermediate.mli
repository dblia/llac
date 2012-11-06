(* Interface file for intermediate code generator *)

(* New datatype definitions for quadruples handling *)
(* Operator type *)
type operator_t =
  | O_Unit | O_Endu
  | O_Plus | O_Minus | O_Mult | O_Div | O_Mod | O_Pow
  | O_Differ | O_Equal | O_NEqual | O_Gt | O_Lt | O_Ge | O_Le
  | O_Andlogic | O_Orlogic | O_Not
  | O_Assign
  | O_Array
  | O_Jump | O_Ifjump | O_Label
  | O_Call | O_Par | O_Ret

(* Value pass mode type *)
type pass_mode_t = V | R | RET

(* Operand type *)
type operand_t =
  | Int of int
  | Char of char
  | True of bool
  | False of bool
  | String of string
  | Invalid             (* No position specified, PLACE initializer *)
  | Label of int                        (* Number of quad for jumps *)
  | Pass of pass_mode_t             (* In case of parameter passing *)
  | Nil
  | Bachpatch
  | Entry of Symbol.entry
  (* The type is needed in order to ensure that a function returns the
   * correct type *)
  | Result of Types.ty
  (* Pointers need two different type representations, hence the second 
   * field denoting the type of the pointed value. *)
  | Pointer of Symbol.entry * Types.ty

(* quad type definition *)
type quadruple_t = {
  label : int;
  op    : operator_t;
  mutable op1 : operand_t;
  mutable op2 : operand_t;
  mutable op3 : operand_t; (* for backpatching *)
}

(* Semantics value struct:
  * This struct is used as a return value by expressions in the semantic
  * analysis of llamac project. The sem_val struct consists of the information
  * about the expression (value_type and expr_type) and about the intermediate
  * code to be generated (PLACE, NEXT, TRUE and FALSE attribute variables ). *)
type value_type = Lval | Rval | Dummy

type sem_val = {
  val_type  : value_type;
  expr_type : Types.ty;
  mutable place  : operand_t;
  mutable next   : int list;
  mutable true_  : int list;
  mutable false_ : int list;
}

(* Auxilary subroutines for quadruples handling *)
(* Returns the number of the next quad *)
val nextQuad : unit -> int ;;

(* Generates new quad and increases quadNext number *)
val genQuad : operator_t -> operand_t -> operand_t -> operand_t -> quadruple_t ;;

(* Generates and returns a new temporary variable *)
val newTemp : Error.finfo -> Types.ty -> Symbol.entry ;;

(* Returns the the type of the n-th parameter of subprogramm e *)
val paramType : Symbol.entry -> int -> Types.ty ;;

(*

(* Quad storage and handling functions *)
(* List of quadruples *)
val quad_list : quadruple_t list ref ;;

(* Get the quads (list needs reversal because of the way we store the quads) *)
val get_quads : unit -> quadruple_t list ;;

(* Add a quad to a list *)
val add_quad : quadruple_t -> unit ;;

(* Backpatching:
 * Replace all 'backpatch' operands in quads with labels in l to z *)
val backpatch : int list -> int -> unit ;;
*)
