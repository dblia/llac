(* Abstract syntax tree produced by parsing *)

open Types
open Error

(* Type expressions for the llama language *)

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

let get_name_of_prog = function
      PROGRAM (l, t) -> (l, t)
;;

(*
let ast_get_finfo = function
    L_Let     (fi, _) -> fi
  | L_LetRec  (fi, _) -> fi
  | TD_Type   (fi, _) -> fi
  | TD_TDefId (fi, _, _) -> fi
  | TD_Constr (fi, _, _) -> fi
  | VAR_Id    (fi, _, _, _, _) -> fi
  | VAR_MutId (fi, _, _, _) -> fi
*)
let get_info_expr = function
    E_Unit         fi -> fi 
  | E_True         fi -> fi
  | E_False        fi -> fi
  | E_LitInt      (fi, _) -> fi
  | E_LitChar     (fi, _) -> fi
  | E_LitFloat    (fi, _) -> fi
  | E_LitId       (fi, _) -> fi
  | E_LitConstr   (fi, _) -> fi
  | E_LitString   (fi, _) -> fi
  | E_UPlus       (fi, _) -> fi
  | E_UFPlus      (fi, _) -> fi
  | E_UMinus      (fi, _) -> fi
  | E_UFMinus     (fi, _) -> fi
  | E_Not         (fi, _) -> fi
  | E_Deref       (fi, _) -> fi
  | E_Delete      (fi, _) -> fi
  | E_Block       (fi, _) -> fi
  | E_Plus        (fi, _, _) -> fi
  | E_FPlus       (fi, _, _) -> fi
  | E_Minus       (fi, _, _) -> fi
  | E_FMinus      (fi, _, _) -> fi
  | E_Mul         (fi, _, _) -> fi
  | E_FMul        (fi, _, _) -> fi
  | E_Div         (fi, _, _) -> fi
  | E_FDiv        (fi, _, _) -> fi
  | E_Mod         (fi, _, _) -> fi
  | E_Pow         (fi, _, _) -> fi
  | E_Eq          (fi, _, _) -> fi
  | E_Differ      (fi, _, _) -> fi
  | E_Equal       (fi, _, _) -> fi
  | E_NEqual      (fi, _, _) -> fi
  | E_Lt          (fi, _, _) -> fi
  | E_Gt          (fi, _, _) -> fi
  | E_Leq         (fi, _, _) -> fi
  | E_Geq         (fi, _, _) -> fi
  | E_Andlogic    (fi, _, _) -> fi
  | E_Orlogic     (fi, _, _) -> fi
  | E_Assign      (fi, _, _) -> fi
  | E_Semicolon   (fi, _, _) -> fi
  | E_While       (fi, _, _) -> fi
  | E_Match       (fi, _, _) -> fi
  | E_IfStmt      (fi, _, _, _) -> fi
  | E_LetIn       (fi, _, _) -> fi
  | E_Dim         (fi, _, _) -> fi
  | E_New         (fi, _) -> fi
  | E_Call        (fi, _, _) -> fi
  | E_Constructor (fi, _, _) -> fi
  | E_ArrayEl     (fi, _, _) -> fi
  | E_For         (fi, _, _, _, _, _) -> fi
;;
(*
  | P_True         fi -> fi
  | P_False        fi -> fi
  | P_LitId       (fi, _) -> fi
  | P_LitChar     (fi, _) -> fi
  | P_LitFloat    (fi, _) -> fi
  | P_Plus        (fi, _) -> fi
  | P_FPlus       (fi, _) -> fi
  | P_Minus       (fi, _) -> fi
  | P_FMinus      (fi, _) -> fi
  | P_Clause      (fi, _, _) -> fi
  | P_LitConstr   (fi, _, _) -> fi
;;
*)
