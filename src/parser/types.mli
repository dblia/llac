(* Basic and auxilary AST types used by ast.ml and parser.ml  *)

type ty =
    TY_Unit
  | TY_Int
  | TY_Float
  | TY_Bool
  | TY_Char
  | TY_Ref of ty
  | TY_Array of int * ty
  | TY_UserDef of string
  | TY_Function of ty * ty
;;

type for_info = UPTO | DOWNTO ;;