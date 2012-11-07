
open Printf

open Identifier
open Symbol
open Error
open Types

(* New datatype definitions for quadruples handling *)
type operator_t =
  | O_Unit | O_Endu
  | O_Plus | O_Minus | O_Mult | O_Div | O_Mod | O_Pow
  | O_Differ | O_Equal | O_NEqual | O_Gt | O_Lt | O_Ge | O_Le
  | O_Andlogic | O_Orlogic | O_Not
  | O_Assign
  | O_Array
  | O_Jump | O_Ifjump | O_Label
  | O_Call | O_Par | O_Ret

type pass_mode_t = V | R | RET

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
  | Entry of Symbol.entry
  (* The type is needed in order to ensure that a function returns the
   * correct type *)
  | Result of Types.ty
  (* Pointers need two different type representations, hence the second
   * field denoting the type of the pointed value. *)
  | Pointer of Symbol.entry * Types.ty

type quadruple_t = {
  label : int;
  op    : operator_t;
  mutable op1 : operand_t;
  mutable op2 : operand_t;
  mutable op3 : operand_t; (* for backpatching *)
}

(* Semantics value struct: *)
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
let nextQuad () =
  !Symbol.quadNext

let genQuad op x y z =
  let quadruple = {
    label = nextQuad ();
    op  = op;
    op1 = x;
    op2 = y;
    op3 = z;
  } in
  incr Symbol.quadNext;
  quadruple

let newTemp =
  Symbol.newTemporary

let entry_parameter_info e n =
  let fname = id_name e.entry_id in
  match e.entry_info with
  | ENTRY_function f ->
    begin try
      let p = List.nth f.function_paramlist n in
      match p.entry_info with
      | ENTRY_parameter pi -> pi
      | _ ->
        printf "Non parameter in parameter list for function %s." fname;
        raise (Exit 4)
     with (Failure "nth") ->
      printf "Invalid parameter request (%d) for function %s." n fname;
      raise (Exit 4)
    end
   | _ ->
     printf "Invalid entry type, function expected for %s." fname;
     raise (Exit 4)

let paramType en nth =
  (entry_parameter_info en nth).parameter_type

let paramMode en nth =
  (entry_parameter_info en nth).parameter_mode

let isFunction en =
  match en.entry_info with
  | ENTRY_function _ -> true
  | _ -> false

let funcResult en =
  match en.entry_info with
  | ENTRY_function fi -> fi.function_result
  | _ ->
      printf "isFunction: Invalid argument";
      raise (Exit 4)

let sizeOf =
  Types.sizeOfType

let typePtr = function
  | TY_Ref ty -> ty
  | _ as ty ->
      printf "typePtr: invalid argument: %s" (string_of_type ty);
      raise (Exit 4)

let typeArr = function
  | TY_Array (sz, et) -> et
  | _ as ty ->
      printf "typeArr: invalid argument: %s" (string_of_type ty);
      raise (Exit 4)

(* Quad storage and handling functions *)
let quad_list = ref []

let get_quads () =
  List.rev !quad_list

let add_quad q =
  quad_list := q :: !quad_list

let backpatch l z =
  let iter quad =
    if List.mem quad.label l then
      match quad.op3 with
      | Backpatch -> quad.op3 <- Label z
      | _ ->
          printf "backapatch: invalid backpatch request";
          raise (Exit 4)
  in
  List.iter iter !quad_list

(* Auxilary functions for debugging *)
let str_of_operator = function
    O_Unit     -> "unit"
  | O_Endu     -> "endu"
  | O_Plus     -> "+"
  | O_Minus    -> "-"
  | O_Mult     -> "*"
  | O_Div      -> "/"
  | O_Mod      -> "%"
  | O_Pow      -> "**"
  | O_Differ   -> "<>"
  | O_Equal    -> "=="
  | O_NEqual   -> "!="
  | O_Gt       -> ">"
  | O_Lt       -> "<"
  | O_Ge       -> ">="
  | O_Le       -> "<="
  | O_Andlogic -> "&&"
  | O_Orlogic  -> "||"
  | O_Not      -> "not"
  | O_Assign   -> ":="
  | O_Array    -> "array"
  | O_Jump     -> "jump"
  | O_Ifjump   -> "ifb"
  | O_Label    -> "label"
  | O_Call     -> "call"
  | O_Par      -> "par"
  | O_Ret      -> "ret"

let str_of_pm = function
    V   -> "V"
  | R   -> "R"
  | RET -> "RET"

let str_of_operand = function
    Unit             -> "unit"
  | True             -> "true"
  | False            -> "false"
  | Int i            -> string_of_int i
  | Float f          -> string_of_float f
  | Char c           -> "'" ^ Char.escaped c ^ "'"
  | String s         -> "" ^ s ^ "" 
  | Invalid          -> "invalid"
  | Label i          -> string_of_int i
  | Pass pm          -> str_of_pm pm
  | Backpatch        -> "*"
  | Empty            -> "-"
  | Entry en         -> id_name en.entry_id
  | Result _         -> "$$"
  | Pointer (en ,_)  -> "[" ^ (id_name en.entry_id)  ^ "]"

let print_quad channel q =
  fprintf channel "%d: %s, %s, %s, %s\n"
  q.label (str_of_operator q.op) (str_of_operand q.op1) (str_of_operand q.op2)
  (str_of_operand q.op3)

let print_quads channel =
  List.iter (print_quad channel) (get_quads ())

let print_quads_to_file channel =
  Pervasives.ignore (List.map (print_quad channel) (get_quads ()))

