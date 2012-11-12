(* Interface file for intermediate code generator *)

(* New datatype definitions for quadruples handling *)
(* Operator type *)
type operator_t =
  | O_Unit | O_Endu
  | O_Plus | O_Minus | O_Mult | O_Div | O_Mod | O_Pow
  | O_SEqual | O_SNEqual | O_Equal | O_NEqual | O_Gt | O_Lt | O_Geq | O_Leq
  | O_Andlogic | O_Orlogic | O_Not
  | O_Assign
  | O_Array
  | O_Jump | O_Ifjump | O_Label
  | O_Call | O_Par | O_Ret

(* Value pass mode type *)
type pass_mode_t = V | R | RET

(* Operand type *)
type operand_t =
    Unit
  | True
  | False
  | Int of int
  | Float of float
  | Char of char
  | String of string
  | Invalid             (* No position specified, PLACE initializer *)
  | Label of int                        (* Number of quad for jumps *)
  | Pass of pass_mode_t             (* In case of parameter passing *)
  | Backpatch
  | Empty
  | New
  | Delete
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
  mutable entry     : Symbol.entry;
  mutable val_type  : value_type;
  mutable expr_type : Types.ty;
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

(* Creates a new quad list by merging the lists [l1;l2;....;ln] given *)
val merge : 'a list list -> 'a list ;;

(* Auxilary function used by paramType and paramMode that returns a
 * parameter_info struct *)
val entry_parameter_info : Symbol.entry -> int -> Symbol.parameter_info ;;

(* Returns the the type of the n-th parameter of the entry given *)
val paramType : Symbol.entry -> int -> Types.ty ;;

(* Returns the parameter_mode of the n-th parameter of the entry given *)
val paramMode : Symbol.entry -> int -> Symbol.pass_mode ;;

(* Checks if the given entry is a function *)
val isFunction : Symbol.entry -> bool ;;

(* Returns the size (in bytes) that required to save the given in memory *)
val sizeOf : Types.ty -> int ;;

(* Returns the type of the TY_Ref type given *)
val typePtr : Types.ty -> Types.ty ;;

(* Returns the type of the TY_Array type given *)
val typeArr : Types.ty -> Types.ty ;;

(* Quad storage and handling functions *)
(* List of quadruples *)
val quad_list : quadruple_t list ref ;;

(* Get the quads (list needs reversal because of the way we store the quads) *)
val get_quads : unit -> quadruple_t list ;;

(* Add a quad to a list *)
val add_quad : quadruple_t -> unit ;;

(* Backpatching:
 * Replace all 'backpatch' operands in quads with labels in l with z *)
val backpatch : int list -> int -> unit ;;

(* Auxilary functions for debbuging *)
val str_of_operator : operator_t -> string ;;
val str_of_pm       : pass_mode_t -> string ;;
val str_of_operand  : operand_t -> string ;;

val print_quad  : out_channel -> quadruple_t -> unit ;;
val print_quads_to_file : out_channel -> unit ;;
