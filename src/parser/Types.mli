type typ = 
    TYPE_unit 
  | TYPE_int
  | TYPE_float
  | TYPE_char
  | TYPE_bool
  | TYPE_ref of typ
  | TYPE_array of typ * int          
  | TYPE_fun of typ * typ 
  | TYPE_userdef of string

  (*
val sizeOfType : typ -> int
val equalType : typ -> typ -> bool
*)
