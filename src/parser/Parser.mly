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
    open Printf
%}
   
/* Ocamlyacc declarations  */
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
%token T_plus 
%token T_minus 
%token T_mul 
%token T_div 
%token T_fplus
%token T_fminus 
%token T_fmul 
%token T_fdiv 
%token T_pow  
%token T_bar 
%token T_semicolon 
%token T_andlogic
%token T_orlogic
%token T_differ 
%token T_lt  
%token T_gt  
%token T_le  
%token T_ge  
%token T_equal  
%token T_nequal
%token T_assign
%token T_lparen 
%token T_rparen
%token T_lbrack 
%token T_rbrack
%token T_comma
%token T_colon

%%

/* Grammar rules  */

%%

(* Trailer - additional Ocaml code *)
