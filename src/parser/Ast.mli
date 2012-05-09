open Types

type ast_program = 
  | P_prog of ast_pdef list

and ast_pdef = 
    P_letdef of ast_letdef 
  | P_typedef of ast_typedef

and ast_letdef = 
    LD_let of ast_def list
  | LD_letrec of ast_def list

and ast_typedef = 
  | TD_type of ast_tdef list

and ast_def = 
    DEF_invariable of string * ast_par list * Types.typ * ast_expr
  | DEF_mutable of string * ast_expr list * Types.typ

and ast_tdef = 
  | TDEF_tdef of string * ast_constr list

and ast_par = 
   | PAR_id of string * Types.typ

and ast_expr = 
    E_intConst of int
  | E_floatConst of float 
  | E_charConst of char
  | E_string of string
  | E_id of string
  | E_true
  | E_false
  | E_unit
  | E_uminus of ast_expr
  | E_ufminus of ast_expr
  | E_deref of ast_expr 
  | E_not of ast_expr
  | E_delete of ast_expr
  | E_plus of ast_expr * ast_expr
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
  | E_sequence of ast_expr * ast_expr 
  | E_assign of ast_expr * ast_expr
  | E_letin of ast_letdef * ast_expr
  | E_new of Types.typ
  | E_dim of int * string
  | E_app of string * ast_expr list
  | E_constr of string * ast_expr list
  | E_arrayEl of string * ast_expr list
  | E_ifStmt of ast_expr * ast_expr 
  | E_ifElse of ast_expr * ast_expr * ast_expr
  | E_while of ast_expr * ast_expr
  | E_for of toDown_flag * string * ast_expr * ast_expr * ast_expr 
  | E_match of ast_expr * ast_clause list

and ast_constr = 
  | C_constr of string * Types.typ list

and ast_clause = 
  | C_clause of ast_pattern * ast_expr

and ast_pattern = 
  | PAT_int of int
  | PAT_float of float
  | PAT_char of char
  | PAT_bool of bool
  | PAT_id of string
  | PAT_constr of string * ast_pattern list

and toDown_flag = TO | DOWNTO
;;

val getTypeOf : ast_program -> ast_pdef list ;;
