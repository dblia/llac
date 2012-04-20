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

%start program
%type <unit> program
 

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

tdef_list : /* nothing */ { () }
          | T_and tdef tdef_list { () }

def : T_cname par_list T_colon typ T_eq expr { () }
    | T_cname par_list T_eq expr { () }
    | T_mutable T_cname T_lbrack expr expr_comm_list T_rbrack T_colon typ { () }
    | T_mutable T_cname T_colon typ { () }
    | T_mutable T_cname T_lbrack expr expr_comm_list T_rbrack { () }
    | T_mutable T_cname { () }

par_list : /* nothing */ { () }
         | par par_list { () }

expr_comm_list : /* nothing */ { () }
          | T_comma expr expr_comm_list { () }

typedef : T_type tdef tdef_list { () }

tdef : T_cname T_eq constr constr_list { () }

constr_list : /* nothing */ { () }
            |  T_pipe constr constr_list { () }

constr : T_constructor T_of typ typ_list { () }
       | T_constructor { () }

typ_list : /* nothing */ { () }
         | typ typ_list { () }

par : T_cname { () }
    | T_lparen T_cname T_colon typ T_rparen { () }

typ : T_unit { () }
    | T_int  { () }
    | T_char { () }
    | T_bool { () }
    | T_float { () }
    | T_lparen typ T_rparen { () }
    | typ T_gives typ { () }
    | typ T_ref { () }
    | T_array T_lbrack T_mul mul_list T_rbrack T_of typ { () }
    | T_array T_of typ { () }
    | T_cname { () }

mul_list : /* nothing */ { () }
         | T_comma T_mul mul_list { () }

expr  /* (* constants *) */ 
     : T_intnum { () }
     | T_floatnum { () }
     | T_cchar { () }
     | T_string { () }
     | T_true { () }
     | T_false { () }
     | T_lparen T_rparen { () }

     /* (* parentheses *) */
     | T_lparen expr T_rparen { () }

     /* (* unary operators *) */
     | T_plus expr { () }
     | T_minus expr { () }
     | T_fplus expr { () }
     | T_fminus expr { () }
     | T_bar expr { () }
     | T_not expr { () }

     /* (* binary operators *) */
     | expr T_plus expr { () }
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
     | expr T_semicolon expr { () }
     | expr T_assign expr { () }

     /* (* ids *) */
     | T_cname expr_list { () }
     | T_constructor expr_list { () }
     | T_cname T_lbrack expr expr_comm_list T_rbrack { () }
     
     /* (* keyword-oriented *) */
     | T_dim T_intnum T_cname { () }
     | T_dim T_cname  { () }
     | T_new typ{ () }
     | T_delete expr { () }
     | letdef T_in expr { () }
     | T_begin expr T_end { () }
     | T_if expr T_then expr T_else expr { () }
     | T_if expr T_then expr { () }
     | T_while expr T_do expr T_done { () }
     | T_for T_cname T_eq expr T_to expr T_do expr T_done { () }
     | T_for T_cname T_eq expr T_downto expr T_do expr T_done { () }
     | T_match expr T_with clause clause_list T_end { () }

expr_list : /* nothing */ { () }
          | expr expr_comm_list { () }

clause_list : /* nothing */ { () }
            | T_pipe clause clause_list { () }

clause : pattern T_gives expr { () }

pattern : T_plus T_intnum { () }
        | T_minus T_intnum { () }
        | T_fplus T_floatnum { () }
        | T_fminus T_floatnum { () }
        | T_cchar { () }
        | T_true { () }
        | T_false { () }
        | T_cname { () }
        | T_lparen pattern T_rparen { () }
        | T_constructor pattern_list { () }

pattern_list : /* nothing */ { () }
             | pattern pattern_list { () }



%%

(* Trailer - additional Ocaml code *)





