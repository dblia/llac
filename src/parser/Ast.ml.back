type ast_program =
    None 
  | P_letdef of ast_letdef
  | P_typedef of ast_typedef

and ast_letdef =
  | L_let of ast_def list

and ast_typedef =
  | T_type of ast_tdef list

and ast_def =
    D_id of ast_par list * ast_typ * ast_expr
  | D_mutable of ast_comm * ast_typ

and ast_tdef = 
  | TD_id of ast_constr list

and ast_constr = 
  | CON_ID of ast_typ list

and ast_par = 
    PAR_id
  | PAR_args of ast_typ

and ast_typ = 
    TY_unit
  | TY_int
  | TY_char
  | TY_float
  | TY_bool
  | TY_id
  | TY_ref of ast_typ
  | TY_gives of ast_typ * ast_typ
  | TY_array of ast_typ

and ast_comm = 
  | C_and of ast_expr * ast_comm list

and ast_expr = 
    E_plus of ast_expr * ast_expr
  | E_minus of ast_expr * ast_expr
  | E_times of ast_expr * ast_expr
  | E_div of ast_expr * ast_expr
  | E_fplus of ast_expr * ast_expr
  | E_fminus of ast_expr * ast_expr
  | E_ftimes of ast_expr * ast_expr
  | E_fdiv of ast_expr * ast_expr
  | E_mod of ast_expr * ast_expr
  | E_pow of ast_expr * ast_expr
  | E_eq of ast_expr * ast_expr
  | E_differ of ast_expr * ast_expr
  | E_lt of ast_expr * ast_expr
  | E_gt of ast_expr * ast_expr
  | E_le of ast_expr * ast_expr
  | E_ge of ast_expr * ast_expr
  | E_equal of ast_expr * ast_expr
  | E_nequal of ast_expr * ast_expr
  | E_andlogic of ast_expr * ast_expr
  | E_orlogic of ast_expr * ast_expr
  | E_assign of ast_expr * ast_expr
  | E_semicol of ast_expr * ast_expr
  | E_in of ast_letdef * ast_expr
  | E_if of ast_expr * ast_expr * ast_expr
  | E_Uplus of ast_expr
  | E_Uminus of ast_expr
  | E_UFplus of ast_expr
  | E_UFminus of ast_expr
  | E_Unot of ast_expr
  | E_Udelete of ast_expr
  | E_id of ast_atom
  | E_Id of ast_atom

and ast_atom = 
    A_bar of ast_atom
  | A_arrayEl of ast_array

and ast_array = 
    AR_id of ast_comm
  | AR_stmt of ast_stmt

and ast_stmt = 
    ST_new of ast_typ
  | ST_simple of ast_simple

and ast_simple = 
    S_id
  | S_Id
  | S_intnum
  | S_floatnum
  | S_char
  | S_true
  | S_false
  | S_unit
  | S_dim
  | S_match of ast_expr * ast_clause list
  | S_block of ast_expr list
  | S_while of ast_expr * ast_expr list
  | S_for of ast_expr * ast_expr * ast_expr list

and ast_clause = 
  | C_gives of ast_expr * ast_expr

