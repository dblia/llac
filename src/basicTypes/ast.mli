(* Abstract syntax tree Interface *)
(* Type expressions for the llama language *)

open Error

type ast_prog =
    PROGRAM of ast_letdef list * ast_typedef list

and ast_letdef =
    L_Let of finfo * ast_vardef list
  | L_LetRec of finfo * ast_vardef list

and ast_typedef =
    TD_Type of finfo * ast_typedef list
  | TD_TDefId of finfo * string * ast_typedef list
  | TD_Constr of finfo * string * Types.ty list option

and ast_vardef =
    VAR_Id of finfo * string * ast_vardef list * Types.ty option * ast_expr
  | VAR_MutId of finfo * string * Types.ty option * ast_expr list option
(*  | VAR_Formal of finfo * string * Types.ty option *)

and ast_expr =
    E_Unit        of finfo
  | E_True        of finfo
  | E_False       of finfo
  | E_LitInt      of finfo * int
  | E_LitChar     of finfo * char
  | E_LitFloat    of finfo * float
  | E_LitId       of finfo * string
  | E_LitConstr   of finfo * string
  | E_LitString   of finfo * string
  | E_UPlus       of finfo * ast_expr
  | E_UFPlus      of finfo * ast_expr
  | E_UMinus      of finfo * ast_expr
  | E_UFMinus     of finfo * ast_expr
  | E_Not         of finfo * ast_expr
  | E_Deref       of finfo * ast_expr
  | E_Delete      of finfo * ast_expr
  | E_Block       of finfo * ast_expr
  | E_Plus        of finfo * ast_expr * ast_expr
  | E_FPlus       of finfo * ast_expr * ast_expr
  | E_Minus       of finfo * ast_expr * ast_expr
  | E_FMinus      of finfo * ast_expr * ast_expr
  | E_Mul         of finfo * ast_expr * ast_expr
  | E_FMul        of finfo * ast_expr * ast_expr
  | E_Div         of finfo * ast_expr * ast_expr
  | E_FDiv        of finfo * ast_expr * ast_expr
  | E_Mod         of finfo * ast_expr * ast_expr
  | E_Pow         of finfo * ast_expr * ast_expr
  | E_Eq          of finfo * ast_expr * ast_expr
  | E_Differ      of finfo * ast_expr * ast_expr
  | E_Equal       of finfo * ast_expr * ast_expr
  | E_NEqual      of finfo * ast_expr * ast_expr
  | E_Lt          of finfo * ast_expr * ast_expr
  | E_Gt          of finfo * ast_expr * ast_expr
  | E_Leq         of finfo * ast_expr * ast_expr
  | E_Geq         of finfo * ast_expr * ast_expr
  | E_Andlogic    of finfo * ast_expr * ast_expr
  | E_Orlogic     of finfo * ast_expr * ast_expr
  | E_Assign      of finfo * ast_expr * ast_expr
  | E_Semicolon   of finfo * ast_expr * ast_expr
  | E_While       of finfo * ast_expr * ast_expr
  | E_Match       of finfo * ast_expr * ast_pattern list
  | E_IfStmt      of finfo * ast_expr * ast_expr * ast_expr option
  | E_LetIn       of finfo * ast_letdef * ast_expr
  | E_Dim         of finfo * int option * string
  | E_New         of finfo * Types.ty
  | E_Call        of finfo * string * ast_expr list
  | E_Constructor of finfo * string * ast_expr list
  | E_ArrayEl     of finfo * string * ast_expr list
  | E_For         of finfo * string * Types.for_info * ast_expr * ast_expr * ast_expr

and ast_pattern =
    P_True      of finfo
  | P_False     of finfo
  | P_LitId     of finfo * string
  | P_LitChar   of finfo * char
  | P_LitFloat  of finfo * float
  | P_Plus      of finfo * int
  | P_FPlus     of finfo * float
  | P_Minus     of finfo * int
  | P_FMinus    of finfo * float
  | P_Clause    of finfo * ast_pattern * ast_expr
  | P_LitConstr of finfo * string * ast_pattern list
;;

val get_name_of_prog : ast_prog -> ast_letdef list * ast_typedef list ;;
