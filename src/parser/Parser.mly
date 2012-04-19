%{
(***************************************************************
 *
 * Parser for Llama Language
 * Authors: Bliablias Dimitrios, Koukoutos Emmanouhl
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

%token T_eof

%%

/* (* Grammar rules *) */

%%

(* Trailer - additional Ocaml code *)
