(* Abstract Syntax Tree for llama *)
open Types

type ast_prog = 
    PROGRAM of ast_letdef list * ast_typedef list

and ast_letdef =
    L_Let of ast_vardef list
  | L_LetRec of ast_vardef list

and ast_typedef =
    TD_Type of ast_typedef list
  | TD_TDefId of string * ast_typedef list
  | TD_Constr of string * Types.ty list option

and ast_vardef =
    VAR_Id of string * ast_vardef list * Types.ty option * ast_expr
  | VAR_MutId of string * Types.ty option * ast_expr list option
  | VAR_Formal of string * Types.ty option

and ast_expr =
    E_Unit
  | E_True
  | E_False
  | E_LitInt      of int
  | E_LitChar     of char
  | E_LitFloat    of float
  | E_LitId       of string
  | E_LitConstr   of string
  | E_LitString   of string
  | E_UPlus       of ast_expr
  | E_UFPlus      of ast_expr
  | E_UMinus      of ast_expr
  | E_UFMinus     of ast_expr
  | E_Not         of ast_expr
  | E_Deref       of ast_expr
  | E_Delete      of ast_expr
  | E_Block       of ast_expr
  | E_Plus        of ast_expr * ast_expr
  | E_FPlus       of ast_expr * ast_expr
  | E_Minus       of ast_expr * ast_expr
  | E_FMinus      of ast_expr * ast_expr
  | E_Mul         of ast_expr * ast_expr
  | E_FMul        of ast_expr * ast_expr
  | E_Div         of ast_expr * ast_expr
  | E_FDiv        of ast_expr * ast_expr
  | E_Mod         of ast_expr * ast_expr
  | E_Pow         of ast_expr * ast_expr
  | E_Eq          of ast_expr * ast_expr
  | E_Differ      of ast_expr * ast_expr
  | E_Equal       of ast_expr * ast_expr
  | E_NEqual      of ast_expr * ast_expr
  | E_Lt          of ast_expr * ast_expr
  | E_Gt          of ast_expr * ast_expr
  | E_Leq         of ast_expr * ast_expr
  | E_Geq         of ast_expr * ast_expr
  | E_Andlogic    of ast_expr * ast_expr
  | E_Orlogic     of ast_expr * ast_expr
  | E_Assign      of ast_expr * ast_expr
  | E_Semicolon   of ast_expr * ast_expr
  | E_While       of ast_expr * ast_expr
  | E_Match       of ast_expr * ast_pattern list
  | E_IfStmt      of ast_expr * ast_expr * ast_expr option
  | E_LetIn       of ast_letdef * ast_expr
  | E_Dim         of int option * string
  | E_New         of Types.ty
  | E_Call        of string * ast_expr list
  | E_Constructor of string * ast_expr list
  | E_ArrayEl     of string * ast_expr list
  | E_For         of string * ast_expr * ast_expr * ast_expr

and ast_pattern =
    P_True
  | P_False
  | P_LitId     of string
  | P_LitChar   of char
  | P_LitFloat  of float
  | P_Plus      of int
  | P_FPlus     of float
  | P_Minus     of int
  | P_FMinus    of float
  | P_Clause    of ast_pattern * ast_expr
  | P_LitConstr of string * ast_pattern list
;;

(* Pretty Printing Function For Ast *)
let pr = Format.print_string

let rec pp_letdef =
  function
    | L_Let vdefs           -> pr "let"; pp_vardefs vdefs
    | L_LetRec vdefs        -> pr "let rec"; pp_vardefs vdefs

and pp_typedef =
  function
    | TD_Type ts            -> pr "type"; pp_typedefs ts
    | TD_TDefId (s, ts)     -> pr s; pr "="; pp_typedefs ts
    | TD_Constr (s, tys)    -> pr s;
                               (match tys with
                                | None -> ()
                                | Some _tys -> pr "of"; pp_types _tys)

and pp_typedefs =
  function
    | [] -> ()
    | (hd :: tl) -> pp_typedef hd; pp_typedefs tl

and pp_vardef =
  function
    | VAR_Id (s, vs, t, e)  -> pr s; pp_vardefs vs;
                               (match t with
                                | None -> ()
                                | Some ty -> pr ":"; pp_type ty);
                               pr "="; pp_expr e
    | VAR_MutId (s, t, es)  -> pr "mutable "; pr s;
                               (match es with
                                | None -> ()
                                | Some _es -> pr "["; pp_com_exprs _es; pr "]");
                               pr " : ";
                               (match t with
                                | None -> ()
                                | Some ty -> pr ":"; pp_type ty)
    | VAR_Formal (s, ty)    -> pr s;
                               (match ty with
                                | None -> ()
                                | Some _ty -> pr ":"; pp_type _ty)

and pp_vardefs =
  function
    | [] -> ()
    | (hd :: tl) -> pp_vardef hd; pp_vardefs tl

and pp_expr =
  function
    | E_Unit                -> ()
    | E_True                -> pr "true"
    | E_False               -> pr "false"
    | E_LitInt i            -> print_int i
    | E_LitChar c           -> print_char c
    | E_LitFloat f          -> print_float f
    | E_LitId id            -> pr id
    | E_LitConstr con       -> pr con
    | E_LitString s         -> pr s
    | E_UPlus e             -> pr " + "; pp_expr e
    | E_UMinus e            -> pr " + "; pp_expr e
    | E_UFPlus e            -> pr " +. "; pp_expr e
    | E_UFMinus e           -> pr " -. "; pp_expr e
    | E_Not e               -> pr "Not "; pp_expr e
    | E_Deref e             -> pr "!"; pp_expr e
    | E_Delete e            -> pr "Delete"; pp_expr e
    | E_Block e             -> pr "begin\n"; pp_expr e; pr "\nend\n"
    | E_Plus (e1, e2)       -> pp_expr e1; pr " + "; pp_expr e2
    | E_FPlus (e1, e2)      -> pp_expr e1; pr " +. "; pp_expr e2
    | E_Minus (e1, e2)      -> pp_expr e1; pr " _ "; pp_expr e2
    | E_FMinus (e1, e2)     -> pp_expr e1; pr " _. "; pp_expr e2
    | E_Mul (e1, e2)        -> pp_expr e1; pr " * "; pp_expr e2
    | E_FMul (e1, e2)       -> pp_expr e1; pr " *. "; pp_expr e2
    | E_Div (e1, e2)        -> pp_expr e1; pr " / "; pp_expr e2
    | E_FDiv (e1, e2)       -> pp_expr e1; pr " /. "; pp_expr e2
    | E_Mod (e1, e2)        -> pp_expr e1; pr " % "; pp_expr e2
    | E_Pow (e1, e2)        -> pp_expr e1; pr " ^ "; pp_expr e2
    | E_Eq (e1, e2)         -> pp_expr e1; pr " = "; pp_expr e2
    | E_Differ (e1, e2)     -> pp_expr e1; pr " <> "; pp_expr e2
    | E_Equal (e1, e2)      -> pp_expr e1; pr " == "; pp_expr e2
    | E_NEqual (e1, e2)     -> pp_expr e1; pr " != "; pp_expr e2
    | E_Lt (e1, e2)         -> pp_expr e1; pr " < "; pp_expr e2
    | E_Gt (e1, e2)         -> pp_expr e1; pr " > "; pp_expr e2
    | E_Leq (e1, e2)        -> pp_expr e1; pr " <= "; pp_expr e2
    | E_Geq (e1, e2)        -> pp_expr e1; pr " >= "; pp_expr e2
    | E_Andlogic (e1, e2)   -> pp_expr e1; pr " && "; pp_expr e2
    | E_Orlogic (e1, e2)    -> pp_expr e1; pr " || "; pp_expr e2
    | E_Assign (e1, e2)     -> pp_expr e1; pr " := "; pp_expr e2
    | E_Semicolon (e1, e2)  -> pp_expr e1; pr "; "; pp_expr e2
    | E_While (e1, e2)      -> pr "while "; pp_expr e1; pr "do\n\t";
                              pp_expr e2; pr "\ndone\n"
    | E_Match (e, ps)       -> pr "match"; pp_expr e; pr "with"; pp_patterns ps
    | E_IfStmt (e1, e2, e3) -> pr "if"; pp_expr e1; pr "then"; pp_expr e2;
                               (match e3 with
                                | None -> ()
                                | Some e -> pr "else"; pp_expr e)
    | E_LetIn (ldef, e)     -> pp_letdef ldef; pr "in\n\t"; pp_expr e
    | E_Dim (i, s)          -> print_string s; pr "[";
                               (match i with
                                | None -> ()
                                | Some _i -> print_int _i); pr "]"
    | E_New t               -> pr "new"; pp_type t
    | E_Call (s, es)        -> pr s; pr " "; pp_exprs es
    | E_Constructor (s, es) -> pr s; pr " "; pp_exprs es
    | E_ArrayEl (s, es)     -> pr s; pr "["; pp_com_exprs es; pr "]"
    | E_For (s, e1, e2, e3) -> pr "for "; pr " = "; pp_expr e1;
                               pr "to/downto"; pp_expr e2; pr "do\n\t";
                               pp_expr e3; pr "\ndone\n"

and pp_exprs =
  function
    | [] -> ()
    | (hd :: tl) -> pp_expr hd; pp_exprs tl

and pp_com_exprs =
  function
    | [] -> ()
    | (hd :: tl) -> pp_expr hd; pr ","; pp_com_exprs tl

and pp_pattern =
  function
    | P_Clause (p, e)       -> pp_pattern p; pr " -> "; pp_expr e
    | P_True                -> pr "true"
    | P_False               -> pr "false"
    | P_LitId id            -> pr id
    | P_LitChar c           -> print_char c
    | P_LitFloat f          -> print_float f
    | P_Plus i              -> pr " + "; print_int i
    | P_Minus i             -> pr " + "; print_int i
    | P_FPlus f             -> pr " +. "; print_float f
    | P_FMinus f            -> pr " -. "; print_float f
    | P_LitConstr (s, ps)   -> pr s; pp_patterns ps

and pp_patterns =
  function
    | [] -> ()
    | (hd :: tl) -> pp_pattern hd; pp_patterns tl

and pp_type =
  function
    | TY_Unit               -> pr "()"
    | TY_Int                -> pr "int"
    | TY_Float              -> pr "float"
    | TY_Bool               -> pr "bool"
    | TY_Char               -> pr "char"
    | TY_Ref t              -> pr "ref"
    | TY_Array (i, ty)      -> pr "array"
    | TY_UserDef s          -> pr "userdefine"
    | TY_Function (t1, t2)  -> pr "fun"

and pp_types =
  function
    | [] -> ()
    | (hd :: tl) -> pp_type hd; pr "|"; pp_types tl
;;

let get_name_of_prog =
  function
    | PROGRAM (l, t) -> (l, t)
