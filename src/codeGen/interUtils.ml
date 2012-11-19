open Printf

open Identifier
open Symbol
open Error
open Types

(* New datatype definitions for quadruples handling *)
type operator_t =
  | O_Unit | O_Endu
  | O_Plus | O_Minus | O_Mult | O_Div | O_Mod | O_Pow
  | O_SEqual | O_SNEqual | O_Equal | O_NEqual | O_Gt | O_Lt | O_Geq | O_Leq
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
  | New
  | Delete
  | Entry of Symbol.entry
  (* The type is needed in order to ensure that a function returns the
   * correct type *)
  | Result of Types.ty
  (* Pointers need two different type representations, hence the second
   * field denoting the type of the pointed value. *)
  | Pointer of Symbol.entry * Types.ty

type quadruple_t = {
  mutable label : int;
  op    : operator_t;
  mutable op1 : operand_t;
  mutable op2 : operand_t;
  mutable op3 : operand_t; (* for backpatching *)
}

(* Semantics value struct: *)
type value_type = Lval | Rval | Cond | Dummy

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

let rec merge = function
  | [] -> []
  | h :: t -> List.rev_append h (merge t)

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
  | O_SEqual   -> "="
  | O_Equal    -> "=="
  | O_SNEqual  -> "<>"
  | O_NEqual   -> "!="
  | O_Gt       -> ">"
  | O_Lt       -> "<"
  | O_Geq      -> ">="
  | O_Leq      -> "<="
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
  | New              -> "_new"
  | Delete           -> "_delete"
  | Entry en         -> id_name en.entry_id
  | Result _         -> "$$"
  | Pointer (en ,_)  -> "[" ^ (id_name en.entry_id)  ^ "]"

let str_of_val_type = function
    Lval -> "LVal"
  | Rval -> "RVal"
  | Cond -> "Cond"
  | Dummy -> "Dummy"

let print_quad channel q =
  fprintf channel "%d:\t%s, %s, %s, %s\n"
  q.label (str_of_operator q.op) (str_of_operand q.op1) (str_of_operand q.op2)
  (str_of_operand q.op3)

let print_quads_to_file channel  acc =
  Pervasives.ignore (List.map (print_quad channel) acc)

let print_quads_to_file2 channel acc =
  let lab : int ref = ref 1 in
  let print_quad_ channel q =
    q.label <- !lab;
    lab := !lab + 1;
    let op = str_of_operator q.op in
    fprintf channel "%d:\t%s, %s, %s, %s\n"
    q.label op (str_of_operand q.op1) (str_of_operand q.op2)
    (str_of_operand q.op3);
    if op = "endu" then fprintf channel "\n" else ()
  in
  Pervasives.ignore (List.map (print_quad_ channel) acc)

let label_change quad cnt =
  let lab = quad.label in
  match quad.op3 with
  | Label i -> 
      { quad with label = lab; op3 = Label (i - lab + cnt) }
  | _  -> 
      { quad with label = lab }

let rec labels_rebuilt lst acc cnt = 
  match lst with
  | [] -> List.rev acc
  | (hd :: tl) ->
      let newq = label_change hd cnt in
      labels_rebuilt tl (newq :: acc) (cnt + 1)

let separate_quads lst =
  (* lst : quad_list
   * acc : accumulator for the _outer scope and the final result
   * acc1: accumulator for a new function structural unit found
   * acc2: accumulator for the functions structural units *)
  let rec quads_sep lst acc acc1 acc2 flag =
  match lst with
  | [] -> 
      let res = List.append acc2 acc in
      labels_rebuilt res [] 1
  | (hd :: tl) ->
      (* find a unit's (not _outer's) start *)
      if not flag && ((str_of_operator hd.op) = "unit") &&
         ((str_of_operand hd.op1) <> "_outer")
      then quads_sep tl acc (hd :: acc1) acc2 true
      (* find a unit's (not _outer's) end *)
      else if flag && ((str_of_operator hd.op) = "endu") &&
              ((str_of_operand hd.op1) <> "_outer")
      then quads_sep tl acc [] (List.append acc2 (List.rev (hd :: acc1))) false
      (* we are in a unit's body (not _outer's) *)
      else if flag then quads_sep tl acc (hd :: acc1) acc2 true
      (* we are in the _outer scope *)
      else quads_sep tl (List.append acc [hd]) [] acc2 false
  in
  quads_sep lst [] [] [] false

(* prints the entry attributes of the sem_val given *)
let pp_print id sem =
  Printf.printf "Name: %s: Ast_rule: %s, Entry_info: %s, Val_type: %s,
    Place: %s, Scope %d, Type: " (id_name sem.entry.entry_id) id
  (str_of_entry_info sem.entry.entry_info) (str_of_val_type sem.val_type)
  (str_of_operand sem.place) (sem.entry.entry_scope.sco_nesting);
  pretty_type Format.std_formatter sem.expr_type;
  Format.print_newline()
