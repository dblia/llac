(* Abstract Syntax Tree for llama *)
open Types

type ast_letdef =
    L_Let of ast_vardef list
  | L_LetRec of ast_vardef list

and ast_typedef =
    TD_Type of ast_typedef list
  | TD_TDefId of string * ast_typedef list
  | TD_Constr of Types.ty list option

and ast_vardef =
    VAR_Id of string * ast_vardef list * ast_expr
  | VAR_MutId of string * Types.ty option * ast_expr list option
  | VAR_Formal of string * Types.ty

and ast_expr =
    E_Unit        of unit
  | E_True        of bool
  | E_False       of bool
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
    P_True      of bool
  | P_False     of bool
  | P_LitId     of string
  | P_LitChar   of char
  | P_LitFloat  of float
  | P_Plus      of int
  | P_FPlus     of float
  | P_Minus     of int
  | P_FMinus    of float
  | P_LitConstr of ast_pattern list
  | P_Clause    of ast_pattern * ast_expr
;;

(* Pretty Printing Function For Ast *)
let pr = Format.print_string

let rec pp_ast =
  function
    | E_Unit               -> pr "()"
    | E_True               -> pr "true"
    | E_False              -> pr "false"
    | E_LitInt i           -> print_int i
    | E_LitChar c          -> print_char c
    | E_LitFloat f         -> print_float f
    | E_LitId id           -> pr id
    | E_LitConstr con      -> pr con
    | E_LitString s        -> pr s
    | E_UPlus e            -> pr " + "; pp_ast e
    | E_UMinus e           -> pr " + "; pp_ast e
    | E_UFPlus e           -> pr " +. "; pp_ast e
    | E_UFMinus e          -> pr " -. "; pp_ast e
    | E_Not e              -> pr "Not "; pp_ast e
    | E_Deref e            -> pr "!"; pp_ast e
    | E_Block e            -> pr "begin\n" pp_ast e; pr "\nend\n"
    | E_Plus (e1, e2)      -> pp_ast e1; pr " + "; pp_ast e2
    | E_FPlus (e1, e2)     -> pp_ast e1; pr " +. "; pp_ast e2
    | E_Minus (e1, e2)     -> pp_ast e1; pr " _ "; pp_ast e2
    | E_FMinus (e1, e2)    -> pp_ast e1; pr " _. "; pp_ast e2
    | E_Mul (e1, e2)       -> pp_ast e1; pr " * "; pp_ast e2
    | E_FMul (e1, e2)      -> pp_ast e1; pr " *. "; pp_ast e2
    | E_Div (e1, e2)       -> pp_ast e1; pr " / "; pp_ast e2
    | E_FDiv (e1, e2)      -> pp_ast e1; pr " /. "; pp_ast e2
    | E_Mod (e1, e2)       -> pp_ast e1; pr " % "; pp_ast e2
    | E_Pow (e1, e2)       -> pp_ast e1; pr " ^ "; pp_ast e2
    | E_Eq (e1, e2)        -> pp_ast e1; pr " = "; pp_ast e2 
    | E_Differ (e1, e2)    -> pp_ast e1; pr " <> "; pp_ast e2
    | E_Equal (e1, e2)     -> pp_ast e1; pr " == "; pp_ast e2
    | E_NEqual (e1, e2)    -> pp_ast e1; pr " != "; pp_ast e2
    | E_Lt (e1, e2)        -> pp_ast e1; pr " < "; pp_ast e2
    | E_Gt (e1, e2)        -> pp_ast e1; pr " > "; pp_ast e2
    | E_Leq (e1, e2)       -> pp_ast e1; pr " <= "; pp_ast e2
    | E_Geq (e1, e2)       -> pp_ast e1; pr " >= "; pp_ast e2
    | E_Andlogic (e1, e2)  -> pp_ast e1; pr " && "; pp_ast e2
    | E_Orlogic (e1, e2)   -> pp_ast e1; pr " || "; pp_ast e2
    | E_Assign (e1, e2)    -> pp_ast e1; pr " := "; pp_ast e2
    | E_Semicolon (e1, e2) -> pp_ast e1; pr "; "; pp_ast e2
    | E_While (e1, e2)     -> pr "while "; pp_ast e1; pr "do\n\t"
                              pp_ast e2; pr "\ndone\n"

