(* Intermediate code generator in quadruple form *)

open Printf

open Error
open Types
open Symbol
open Identifier

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
  | Int of int
  | Char of char
  | True
  | False
  | String of string
  | Invalid             (* No position specified, PLACE initializer *)
  | Label of int                        (* Number of quad for jumps *)
  | Pass of pass_mode_t             (* In case of parameter passing *)
  | Nil
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
    Int i            -> string_of_int i
  | Char c           -> "'" ^ Char.escaped c ^ "'"
  | True             -> "true"
  | False            -> "false"
  | String s         -> "" ^ s ^ "" 
  | Invalid          -> "invalid"
  | Label i          -> string_of_int i
  | Pass pm          -> str_of_pm pm
  | Nil              -> "nil"
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

  (*
(* Main Intermediate funtions:
 * traverses the ast and informs the appropriate variables and structs *)

(*
let rec interOf = function
    PROGRAM (ldfs, tdfs) ->
      List.iter (fun x -> interOfLetdef x) ldfs;
      List.iter (fun x -> interOfTypedef x) tdfs

and interOfLetdef = function
    L_Let (sem, fi, vl)    -> (* vl: vardefs connected with 'and' keyword *)
  | L_LetRec (sem, fi, vl) -> (* vl: vardefs connected with 'and' keyword *)
and interOfTypedef = function
    TD_Type (sem, fi, tl)       -> (* TODO: Not supported yet *)
      error fi 3 "user defined data types are not supported"
  | TD_TDefId (sem, fi, s, tl)  -> (* TODO: Not supported yet *)
      error fi 3 "user defined data types are not supported"
  | TD_Constr (sem, fi, s, tyl) -> (* TODO: Not supported yet *)
      error fi 3 "user defined data types are not supported"

and interOfVardef rec_flag = function
    VAR_Id (sem, fi, s, varl, e) ->
  | VAR_MutId (sem, fi, s, exprl) ->

and interOfExpr = function
  (* Constants Operators *)
    E_Unit (sem, info)        ->
  | E_True (sem, info)        ->
  | E_False (sem, info)       ->
  | E_LitInt (sem, info, _)   ->
  | E_LitChar (sem, info, _)  ->
  | E_LitFloat (sem, info, _) ->
  | E_LitString (sem, fi, s)  ->
  (* Names (constants, functions, parameters, constructors, expressions) *)
  | E_LitId (sem, fi, id)    ->
  | E_LitConstr (sem, fi, id) -> (* TODO: not supported yet *)
      error fi 3 "user defined date types are not supported"
  (* Unary Arithmetic Operators *)
  | E_UPlus (sem, fi, e)     ->
  | E_UFPlus (sem, fi, e)    ->
  | E_UMinus (sem, fi, e)    ->
  | E_UFMinus (sem, fi, e)   ->
  (* References and assigments *)
  | E_Assign (sem, fi, e1, e2)      -> (* FIXME: check if e1 is l-value *)
  | E_Deref (sem, fi, e)     ->
  (* Memory Dynamic Allocation *)
  | E_New (sem, fi)         ->
  | E_Delete (sem, fi, e)      -> (* FIXME: check that mem was allocated dynamically *)
  (* Binary Integer Arithmetic Operators *)
  | E_Plus (sem, fi, e1, e2)   ->
  | E_Minus (sem, fi, e1, e2)  ->
  | E_Mul (sem, fi, e1, e2)    ->
  | E_Div (sem, fi, e1, e2)    ->
  | E_Mod (sem, fi, e1, e2)     ->
  (* Binary Float Arithmetic Operators *)
  | E_FPlus (sem, fi, e1, e2)  ->
  | E_FMinus (sem, fi, e1, e2) ->
  | E_FMul (sem, fi, e1, e2)   ->
  | E_FDiv (sem, fi, e1, e2)   ->
  | E_Pow (sem, fi, e1, e2)    ->
  (* Structural and Natural Equality Operators *)
  | E_Eq (sem, fi, e1, e2)         ->
  | E_Differ (sem, fi, e1, e2)     ->
  | E_Equal (sem, fi, e1, e2)       ->
  | E_NEqual (sem, fi, e1, e2)      ->
  | E_Lt (sem, fi, e1, e2)          ->
  | E_Gt (sem, fi, e1, e2)          ->
  | E_Leq (sem, fi, e1, e2)         ->
  | E_Geq (sem, fi, e1, e2)         ->
  (* Logical Operators *)
  | E_Not (sem, fi, e)       ->
  | E_Andlogic (sem, fi, e1, e2)    ->
  | E_Orlogic (sem, fi, e1, e2)     ->
  (* Imperative Commands *)
  | E_Block (sem, fi, e)     -> interOfExpr e
  | E_Semicolon (sem, fi, e1, e2)   -> P.ignore(interOfExpr e1); interOfExpr e2
  | E_While (sem, fi, e1, e2)       ->
  | E_For (sem, fi, s, ti, e1, e2, e)  ->
  (* Decomposition of User Defined Types *)
  | E_Match (sem, fi, e, clauses)   ->
      error fi 3 "user defined data types are not supported"
  (* Local definitions *)
  | E_LetIn (sem, fi, ld, e)        -> (* local declarations *)
  (* If statement *)
  | E_IfStmt (sem, fi, e, e1, _e2)   ->
  (* Array Elements and Dimensions *)
  | E_Dim (sem, fi, i, s)           ->
  | E_ArrayEl (sem, fi, s, el, el_len)      ->
  (* Function and Constructor call *)
  | E_Call (sem, fi, s, el)         ->
  | E_ConstrCall (sem, fi, s, el)  -> (* TODO: not supported yet *)
      error fi 3 "user defined data types are not supported"


and interOfPattern = function
    P_True (sem, info)             -> TY_Bool
  | P_False (sem, info)            -> TY_Bool
  | P_LitId (sem, fi,id)           ->
      P.ignore (newVariable fi (id_make id) TY_Unit true);
      TY_Unit
  | P_LitChar (sem, fi, c)         -> TY_Char
  | P_LitFloat (sem, fi, f)        -> TY_Char
  | P_Plus (sem, fi, _)            -> TY_Int
  | P_FPlus (sem, fi, _)           -> TY_Float
  | P_Minus (sem, fi, _)           -> TY_Int
  | P_FMinus  (sem, fi, _)         -> TY_Float
  | P_LitConstr (sem, fi, s, patl) -> (* TODO: not supported yet *)
      error fi 3 "user defined data types are not supported"
  | _                 -> err "Wrong pattern form"
;;
*)
*)
