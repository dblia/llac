type ast_program =
    None 
  | P_letdef of ast_letdef 
  | P_typedef of ast_typedef

and ast_letdef = 
  | L_let of int
  
and ast_typedef = 
  | TY_type of int

