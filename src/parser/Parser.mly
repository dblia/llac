%{
(***************************************************************
 *
 * Parser for Llama Language
 * Authors: Bliablias Dimitrios, Koukoutos Emmanouil
 *
 * This file is part of Llamac project.
 *
****************************************************************)

(* Header Section *)

open Lexer
(*
type Program =
    | Defs of Def_list

type Def_list = 
	| Nothing
    | De
*)
%}
   
/* (* Ocamlyacc declarations *) */
%token T_eof

%token<int> T_intnum      
%token<char> T_cchar   
%token<float> T_floatnum
%token<string> T_cname
%token<string> T_constructor
%token<string> T_string

%token T_and T_array T_begin T_bool T_char T_delete T_dim T_do T_done T_downto
  T_else T_end T_false T_float T_for T_if T_in T_int T_let T_match T_mod 
  T_mutable T_new T_not T_of T_rec T_ref T_then T_to T_true T_type T_unit 
  T_while T_with 

%token T_gives 
%token T_eq 
%token T_pipe 

%token T_plus T_minus T_mul T_div T_fplus T_fminus T_fmul T_fdiv T_pow  

%token T_bar 
%token T_semicolon 

%token T_andlogic T_orlogic T_differ T_lt T_gt T_le T_ge T_equal T_nequal
%token T_assign

%token T_lparen T_rparen T_lbrack T_rbrack

%token T_comma
%token T_colon

/* (* predecence for type defs *) */
%right T_gives
%nonassoc T_of
%left T_ref

%left T_semicolon

%nonassoc T_assign
%left T_orlogic
%left T_andlogic 
%nonassoc T_eq T_differ T_lt T_gt T_le T_ge T_equal T_nequal
%left T_plus T_minus T_fplus T_fminus 
%left T_mul T_div T_fmul T_fdiv T_mod
%right T_pow


%start program
%type <unit> program
%type <unit> pdef_list
%type <unit> letdef 
%type <unit> def_list
%type <unit> tdef_list
%type <unit> def
%type <unit> par_list
%type <unit> let_expr_comm_list
%type <unit> let_expr
%type <unit> stmt_list
%type <unit> stmt
%type <unit> if_stmt
%type <unit> full_stmt
%type <unit> typedef
%type <unit> tdef 
%type <unit> constr_list
%type <unit> constr
%type <unit> typ_list
%type <unit> par
%type <unit> typ
%type <unit> mul_list
%type <unit> expr
%type <unit> unary_expr 
%type <unit> app 
%type <unit> atom_list
%type <unit> atom 
%type <unit> array_el 
%type <unit> new_stmt 
%type <unit> simple_expr 
%type <unit> clause_list
%type <unit> clause
%type <unit> pattern
%type <unit> simple_pattern_list
 

%%

/* (* Grammar rules *) */
program : pdef_list T_eof { () }

pdef_list : /* nothing */      { () }
          | letdef pdef_list   { () }
          | typedef pdef_list  { () }

letdef : T_let T_rec def def_list { () }
       | T_let def def_list { () }

def_list  : /* nothing */ { () }
          | T_and def def_list { () }

def : T_cname par_list T_colon typ T_eq let_expr { () }
    | T_cname par_list T_eq let_expr { () }
    | T_mutable T_cname T_lbrack let_expr let_expr_comm_list T_rbrack T_colon typ { () }
    | T_mutable T_cname T_colon typ { () }
    | T_mutable T_cname T_lbrack let_expr let_expr_comm_list T_rbrack { () }
    | T_mutable T_cname { () }

par_list : /* nothing */ { () }
         | par par_list { () }

let_expr_comm_list : /* nothing */ { () }
          | T_comma let_expr let_expr_comm_list { () }

typedef : T_type tdef tdef_list { () }

tdef_list : /* nothing */ { () }
          | T_and tdef tdef_list { () }

tdef : T_cname T_eq constr constr_list { () }

constr_list : /* nothing */ { () }
            |  T_pipe constr constr_list { () }

constr : T_constructor T_of typ typ_list { () }
       | T_constructor { () }

typ_list : /* nothing */ { () }
         | typ typ_list { () }

par : T_cname { () }
    | T_lparen T_cname T_colon typ T_rparen { () }

typ : T_array T_lbrack T_mul mul_list T_rbrack T_of simple_typ { () }
    | T_array T_of simple_typ { () }
    

simple_typ: 
      T_unit { () }
    | T_int  { () }
    | T_char { () }
    | T_bool { () }
    | T_float { () }
    | T_lparen simple_typ T_rparen { () }
    | typ T_gives typ { () } 
    | simple_typ T_ref { () }    
    | T_cname { () }

mul_list : /* nothing */ { () }
         | T_comma T_mul mul_list { () }

let_expr: letdef T_in let_expr { () }
        | stmt stmt_list { () }

stmt_list : /* nothing */ { () }
          | T_semicolon stmt stmt_list { () }

stmt: if_stmt { () }
    | T_begin let_expr T_end { () }
    | T_while let_expr T_do let_expr T_done { () }
    | T_for T_cname T_eq let_expr T_to let_expr T_do let_expr T_done { () }
    | T_for T_cname T_eq let_expr T_downto let_expr T_do let_expr T_done { () }
    | expr { () }

if_stmt : T_if stmt T_then full_stmt T_else stmt { () }  /* (* stmt or expr or what ??? *) */
        | T_if stmt T_then stmt { () }

full_stmt : T_if stmt T_then full_stmt T_else full_stmt { () } 
          | expr { () }

expr /* (* binary operators *) */
     : expr T_plus expr { () }
     | expr T_minus expr { () }
     | expr T_mul expr { () }
     | expr T_div expr { () }
     | expr T_fplus expr { () }
     | expr T_fminus expr { () }
     | expr T_fmul expr { () }
     | expr T_fdiv expr { () }
     | expr T_mod expr { () }
     | expr T_pow expr { () }
     | expr T_eq expr { () }
     | expr T_differ expr { () }
     | expr T_lt expr { () }
     | expr T_gt expr { () }
     | expr T_le expr { () }
     | expr T_ge expr { () }
     | expr T_equal expr { () }
     | expr T_nequal expr { () }
     | expr T_andlogic expr { () }
     | expr T_orlogic expr { () }
     | expr T_assign expr { () }
     | unary_expr { () }

unary_expr : T_plus unary_expr { () }
           | T_minus unary_expr { () }
           | T_fplus unary_expr { () }
           | T_fminus unary_expr { () }
           | T_not unary_expr { () }
           | T_delete unary_expr { () }
           | app { () }

app : T_cname atom_list { () } /* (* seperate app from simple ids??? *) */
    | T_constructor atom_list { () }
    | atom { () }

atom_list: /* nothing */ { () }
         | atom atom_list { () }

atom : T_bar atom { () }
     | array_el { () }

array_el : T_cname T_lbrack let_expr let_expr_comm_list T_rbrack { () }
         | new_stmt { () }

new_stmt : T_new typ { () }
         | simple_expr { () }

simple_expr /* (* constants *) */ 
           : T_intnum { () }
           | T_floatnum { () }
           | T_cchar { () }
           | T_string { () }
           | T_true { () }
           | T_false { () }
           | T_lparen T_rparen { () }
           /* (* keyword-oriented *) */
           | T_dim T_intnum T_cname { () }
           | T_dim T_cname  { () }        
           | T_match let_expr T_with clause clause_list T_end { () }
           /* (* parentheses *) */
           | T_lparen let_expr T_rparen { () }

clause_list : /* nothing */ { () }
            | T_pipe clause clause_list { () }

clause : pattern T_gives let_expr { () }

pattern : T_constructor simple_pattern_list { () }
        | simple_pattern { () }

simple_pattern 
        : T_plus T_intnum { () }
        | T_minus T_intnum { () }
        | T_fplus T_floatnum { () }
        | T_fminus T_floatnum { () }
        | T_intnum { () }
        | T_floatnum { () }
        | T_cchar { () }
        | T_true { () }
        | T_false { () }
        | T_cname { () }  
        | T_lparen pattern T_rparen { () }

simple_pattern_list : /* nothing */ { () }
             | simple_pattern simple_pattern_list { () }




