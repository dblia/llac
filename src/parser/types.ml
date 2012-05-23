(****** Types in Llama *****)
type ty =
    TY_Unit
  | TY_Int
  | TY_Float
  | TY_Bool
  | TY_Char
  | TY_Ref of ty
  | TY_Array of int * ty
  | TY_Userdef of string
  | TY_Function of ty * ty
