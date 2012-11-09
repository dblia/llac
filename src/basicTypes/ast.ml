(* Abstract syntax tree produced by parsing *)

open Types
open Error
open InterUtils

(* Type expressions for the llama language *)
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
  | E_LitId       of sem_val * finfo
  | E_LitConstr   of sem_val * finfo
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
  | E_Dim         of sem_val * finfo * int option
  | E_New         of sem_val * finfo
  | E_Call        of sem_val * finfo * ast_expr list
  | E_ConstrCall  of sem_val * finfo * ast_expr list
  | E_ArrayEl     of sem_val * finfo * ast_expr list
  | E_For         of sem_val * finfo * Types.for_info * ast_expr * ast_expr * 
                     ast_expr

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
  | P_LitConstr of sem_val * finfo * ast_pattern list
;;

let get_name_of_prog = function
    PROGRAM (l, t) -> (l, t)
;;

let get_info_expr = function
    E_Unit        (_, fi) -> fi
  | E_True        (_, fi) -> fi
  | E_False       (_, fi) -> fi
  | E_LitInt      (_, fi, _) -> fi
  | E_LitChar     (_, fi, _) -> fi
  | E_LitFloat    (_, fi, _) -> fi
  | E_LitId       (_, fi) -> fi
  | E_LitConstr   (_, fi) -> fi
  | E_LitString   (_, fi, _) -> fi
  | E_UPlus       (_, fi, _) -> fi
  | E_UFPlus      (_, fi, _) -> fi
  | E_UMinus      (_, fi, _) -> fi
  | E_UFMinus     (_, fi, _) -> fi
  | E_Not         (_, fi, _) -> fi
  | E_Deref       (_, fi, _) -> fi
  | E_Delete      (_, fi, _) -> fi
  | E_Block       (_, fi, _) -> fi
  | E_Plus        (_, fi, _, _) -> fi
  | E_FPlus       (_, fi, _, _) -> fi
  | E_Minus       (_, fi, _, _) -> fi
  | E_FMinus      (_, fi, _, _) -> fi
  | E_Mul         (_, fi, _, _) -> fi
  | E_FMul        (_, fi, _, _) -> fi
  | E_Div         (_, fi, _, _) -> fi
  | E_FDiv        (_, fi, _, _) -> fi
  | E_Mod         (_, fi, _, _) -> fi
  | E_Pow         (_, fi, _, _) -> fi
  | E_Eq          (_, fi, _, _) -> fi
  | E_Differ      (_, fi, _, _) -> fi
  | E_Equal       (_, fi, _, _) -> fi
  | E_NEqual      (_, fi, _, _) -> fi
  | E_Lt          (_, fi, _, _) -> fi
  | E_Gt          (_, fi, _, _) -> fi
  | E_Leq         (_, fi, _, _) -> fi
  | E_Geq         (_, fi, _, _) -> fi
  | E_Andlogic    (_, fi, _, _) -> fi
  | E_Orlogic     (_, fi, _, _) -> fi
  | E_Assign      (_, fi, _, _) -> fi
  | E_Semicolon   (_, fi, _, _) -> fi
  | E_While       (_, fi, _, _) -> fi
  | E_Match       (_, fi, _, _) -> fi
  | E_IfStmt      (_, fi, _, _, _) -> fi
  | E_LetIn       (_, fi, _, _) -> fi
  | E_Dim         (_, fi, _) -> fi
  | E_New         (_, fi)       -> fi
  | E_Call        (_, fi, _) -> fi
  | E_ConstrCall  (_, fi, _) -> fi
  | E_ArrayEl     (_, fi, _) -> fi
  | E_For         (_, fi, _, _, _, _) -> fi
;;

