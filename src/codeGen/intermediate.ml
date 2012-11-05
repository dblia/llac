(* Intermediate code generator in quadruple form *)

open Types
open Symbol

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

(* New datatype definitions for quadruples handling *)
type operator = Empty

type operand = Empty

type quad = {
  label : int;
  mutable operator : operator;
  operand1         : operand;
  operand2         : operand;
  mutable operand3 : operand; (* for backpatching *)
}

(* Auxilary subroutines for quadruples handling *)
let nextQuad () =
  !Symbol.quadNext

let genQuad op x y z =
  let quadruple = {
    label = nextQuad ();
    operator = op;
    operand1 = x;
    operand2 = y;
    operand3 = z;
  } in
  incr Symbol.quadNext;
  quadruple

(* Quad storage and handling functions *)
let quad_list = ref []

let get_quads () =
  List.rev !quad_list

let add_quad q =
  quad_list := q :: !quad_list

let backpatch l z =
  let patch quad x =
    quad.operand3 <- Label x in
    let rec backpatch_ l x =
      match l with
      | h :: t ->
          List.iter (fun q ->
            if (h == q.label) then (patch q x; backpatch_ t x)
            else backpatch_ t x) !quad_list
      | [] -> ()
    in
    backpatch_ l z

(* Auxilary functions for debugging *)
let print_quad channel q =
  Printf.fprintf channel "%d: %s, %s, %s, %s\n"
  q.label (str_of_operator q.operator) (str_of_operand q.operand1)
  (str_of_operand q.operand2) (str_of_operand q.operand3)

let print_quads_to_file channel =
  Pervasive.ignore (List.map (print_quad channel) (get_quads ()))

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
