(* Abstract syntax tree produced by parsing *)

open Types

(* Type expressions for the llama language *)

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
  | E_For         of string * Types.for_info * ast_expr * ast_expr * ast_expr

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

let get_name_of_prog =
  function
      PROGRAM (l, t) -> (l, t)
;;
