type typ =
    TYPE_unit
  | TYPE_int
  | TYPE_float
  | TYPE_char
  | TYPE_bool
  | TYPE_ref of typ
  | TYPE_array of typ * int
  | TYPE_fun of typ * typ
  | TYPE_userdef of string  (* ? *)

let rec sizeOfType t =
   match t with
   | TYPE_int            -> 2
   | TYPE_float          -> 2
   | TYPE_char           -> 1
   | TYPE_bool           -> 1
   | TYPE_ref (et)       -> 2
   | TYPE_array (et, sz) -> sz * sizeOfType et
   | _                   -> 0

let rec equalType t1 t2 =
   match t1, t2 with
   | TYPE_array (et1, sz1), TYPE_array (et2, sz2) -> equalType et1 et2
   | _                                            -> t1 = t2
