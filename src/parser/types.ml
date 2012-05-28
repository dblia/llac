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

(* Auxilary functions for the typechecking  *)

let rec sizeOfType t =
 match t with
   TY_Int            -> 2
 | TY_Char           -> 1
 | TY_Array (sz, et) -> sz * sizeOfType et
 | _                 -> 0
;;

let rec equalType t1 t2 =
 match t1, t2 with
   TY_Array (sz1, et1), TY_Array (sz2, et2) -> equalType et1 et2
 | _                                        -> t1 = t2
;;

let isUnit =
  function
      TY_Unit -> true
    | _ -> false
;;

let isBool =
  function
      TY_Bool -> true
    | _ -> false
;;

let isSimpleType =
  function
      TY_Int | TY_Float | TY_Char -> true
    | _ -> false
;;

let isNotArrayFunc =
  function
      TY_Array _ | TY_Function _ -> false
    | _ -> true
