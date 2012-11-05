(* Types interface file *)

type ty =
    TY_Unit
  | TY_Int
  | TY_Float
  | TY_Bool
  | TY_Char
  | TY_Ref of ty
  | TY_Array of int * ty
  | TY_UserDef of string
  | TY_Function of ty list * ty
;;

type for_info = UPTO | DOWNTO ;;

val sizeOfType : ty -> int ;;
val equalType  : ty -> ty -> bool ;;
val string_of_type : ty -> string;;

val isRef  : ty -> bool ;;
val isUnit : ty -> bool ;;
val isBool : ty -> bool ;;
val isSimpleType : ty -> bool ;;
val isNotArrayOrFunc : ty -> bool ;;

val get : 'a option -> 'a ;;
