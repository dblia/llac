(* Abstract syntax tree Interface *)
(* Type expressions for the llama language *)

open Error
open Intermediate

type ast_prog =
    PROGRAM of ast_letdef list * ast_typedef list

and ast_letdef =
    L_Let of sem_val * finfo * ast_vardef list
  | L_LetRec of sem_val * finfo * ast_vardef list

and ast_typedef =
    TD_Type of sem_val * finfo * ast_typedef list
  | TD_TDefId of sem_val * finfo * string * ast_typedef list
  | TD_Constr of sem_val * finfo * string * Types.ty list option

and ast_vardef =
    VAR_Id of sem_val * finfo * string * ast_vardef list * ast_expr
  | VAR_MutId of sem_val * finfo * string * ast_expr list option
(*  | VAR_Formal of sem_val * finfo * string * Types.ty option *)

and ast_expr =
    E_Unit        of sem_val * finfo
  | E_True        of sem_val * finfo
  | E_False       of sem_val * finfo
  | E_LitInt      of sem_val * finfo * int
  | E_LitChar     of sem_val * finfo * char
  | E_LitFloat    of sem_val * finfo * float
  | E_LitId       of sem_val * finfo * string
  | E_LitConstr   of sem_val * finfo * string
  | E_LitString   of sem_val * finfo * string
  | E_UPlus       of sem_val * finfo * ast_expr
  | E_UFPlus      of sem_val * finfo * ast_expr
  | E_UMinus      of sem_val * finfo * ast_expr
  | E_UFMinus     of sem_val * finfo * ast_expr
  | E_Not         of sem_val * finfo * ast_expr
  | E_Deref       of sem_val * finfo * ast_expr
  | E_Delete      of sem_val * finfo * ast_expr
  | E_Block       of sem_val * finfo * ast_expr
  | E_Plus        of sem_val * finfo * ast_expr * ast_expr
  | E_FPlus       of sem_val * finfo * ast_expr * ast_expr
  | E_Minus       of sem_val * finfo * ast_expr * ast_expr
  | E_FMinus      of sem_val * finfo * ast_expr * ast_expr
  | E_Mul         of sem_val * finfo * ast_expr * ast_expr
  | E_FMul        of sem_val * finfo * ast_expr * ast_expr
  | E_Div         of sem_val * finfo * ast_expr * ast_expr
  | E_FDiv        of sem_val * finfo * ast_expr * ast_expr
  | E_Mod         of sem_val * finfo * ast_expr * ast_expr
  | E_Pow         of sem_val * finfo * ast_expr * ast_expr
  | E_Eq          of sem_val * finfo * ast_expr * ast_expr
  | E_Differ      of sem_val * finfo * ast_expr * ast_expr
  | E_Equal       of sem_val * finfo * ast_expr * ast_expr
  | E_NEqual      of sem_val * finfo * ast_expr * ast_expr
  | E_Lt          of sem_val * finfo * ast_expr * ast_expr
  | E_Gt          of sem_val * finfo * ast_expr * ast_expr
  | E_Leq         of sem_val * finfo * ast_expr * ast_expr
  | E_Geq         of sem_val * finfo * ast_expr * ast_expr
  | E_Andlogic    of sem_val * finfo * ast_expr * ast_expr
  | E_Orlogic     of sem_val * finfo * ast_expr * ast_expr
  | E_Assign      of sem_val * finfo * ast_expr * ast_expr
  | E_Semicolon   of sem_val * finfo * ast_expr * ast_expr
  | E_While       of sem_val * finfo * ast_expr * ast_expr
  | E_Match       of sem_val * finfo * ast_expr * ast_pattern list
  | E_IfStmt      of sem_val * finfo * ast_expr * ast_expr * ast_expr option
  | E_LetIn       of sem_val * finfo * ast_letdef * ast_expr
  | E_Dim         of sem_val * finfo * int option * string
  | E_New         of sem_val * finfo
  | E_Call        of sem_val * finfo * string * ast_expr list
  | E_ConstrCall  of sem_val * finfo * string * ast_expr list
  | E_ArrayEl     of sem_val * finfo * string * ast_expr list * int
  | E_For         of sem_val * finfo * string * Types.for_info * ast_expr * ast_expr * ast_expr

and ast_pattern =
    P_True      of sem_val * finfo
  | P_False     of sem_val * finfo
  | P_LitId     of sem_val * finfo * string
  | P_LitChar   of sem_val * finfo * char
  | P_LitFloat  of sem_val * finfo * float
  | P_Plus      of sem_val * finfo * int
  | P_FPlus     of sem_val * finfo * float
  | P_Minus     of sem_val * finfo * int
  | P_FMinus    of sem_val * finfo * float
  | P_Clause    of sem_val * finfo * ast_pattern * ast_expr
  | P_LitConstr of sem_val * finfo * string * ast_pattern list
;;

(* Removes the PROGRAM name and keeps the letdef and userdef lists *)
val get_name_of_prog : ast_prog -> ast_letdef list * ast_typedef list ;;
(* Get file info form the expression given *)
val get_info_expr    : ast_expr -> finfo;;
