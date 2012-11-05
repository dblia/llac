(* Basic and auxilary AST types *)

open Printf

type ty =
    TY_None
  | TY_Unit
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

(* Auxilary functions for the typechecking  *)

let rec sizeOfType t =
 match t with
 | TY_None           -> 0
 | TY_Unit           -> 0
 | TY_Int            -> 2
 | TY_Float          -> 2
 | TY_Bool           -> 1
 | TY_Char           -> 1
 | TY_Ref _          -> 2 (* FIXME: *)
 | TY_Array (sz, et) -> 2 (* FIXME: sz * sizeOfType et *)
 | TY_UserDef _      -> 0 (* FIXME: *)
 | TY_Function _     -> 2 (* FIXME: *)
;;

let rec equalType t1 t2 =
 match t1, t2 with
 | TY_Ref t1, TY_Ref t2                           -> equalType t1 t2
 | TY_Array (sz1, et1), TY_Array (sz2, et2)       -> equalType et1 et2
 | TY_UserDef s1, TY_UserDef s2                   -> s1 = s2
 | TY_Function (ls1, ty1), TY_Function (ls2, ty2) ->
     (* First check return type of functions *)
     (equalType ty1 ty2) &&
     (* Then check their parameter's length and type *)
     (List.length ls1 = List.length ls2) &&
     List.fold_left (fun bl (x, y) -> bl && (equalType x y)) 
        true (List.combine ls1 ls2)
 | _                                              -> t1 = t2
;;

let rec string_of_type = function
    TY_None              -> sprintf "TY_None"
  | TY_Unit              -> sprintf "TY_Unit"
  | TY_Int               -> sprintf "TY_Int"
  | TY_Float             -> sprintf "TY_Float"
  | TY_Bool              -> sprintf "TY_Bool"
  | TY_Char              -> sprintf "TY_Char"
  | TY_Ref ty            -> sprintf "TY_Ref of %s" (string_of_type ty)
  | TY_Array (sz, et)    -> sprintf "TY_Array of %s" (string_of_type et)
  | TY_UserDef s         -> sprintf "TY_UserDef %s" s
  | TY_Function (ls, ty) -> sprintf "TY_Function of %s" (string_of_type ty)
;;

let isRef =
  function
      TY_Ref _ -> true
    | _ -> false
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

let isNotArrayOrFunc =
  function
      TY_Array _ | TY_Function _ -> false
    | _ -> true
;;

exception No_value

let get = function
    None -> raise No_value
  | Some a -> a
;;
