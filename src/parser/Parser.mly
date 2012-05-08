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

%}
   
/* (* Ocamlyacc declarations 
      We first list all the tokens mentioned in the parsing rules below. The
      token names are common for parser and lexical analyzer. *) i
*/

/* (* Identifier and Constant value Tokens *) */
%token <int> T_intnum      
%token <char> T_cchar   
%token <float> T_floatnum
%token <string> T_string
%token <string> T_cname
%token <string> T_constructor

/* (* Keyword Tokens *) */
%token T_and T_array T_begin T_bool T_char T_delete T_dim T_do T_done T_downto
  T_else T_end T_false T_float T_for T_if T_in T_int T_let T_match T_mod 
  T_mutable T_new T_not T_of T_rec T_ref T_then T_to T_true T_type T_unit 
  T_while T_with
  
/* (* Symbolic Tokens *) */
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
%token T_err


/* (* Precedence declarations: The lower the declaration is, the higher it's
      precedence. *)
*/

/* (* Predecence for type defs *) */
%right T_gives
%nonassoc T_of T_array
%left T_ref

/* (* Predecence for expressions *) */
%nonassoc T_in        /* (* let in *) */
%left T_semicolon    
%left T_if T_then 
%nonassoc T_else 
%nonassoc T_assign
%left T_orlogic
%left T_andlogic 
%nonassoc T_eq T_differ T_lt T_gt T_le T_ge T_equal T_nequal
%left T_plus T_minus T_fplus T_fminus 
%left T_mul T_div T_fmul T_fdiv T_mod
%right T_pow

/* (* The starting production of the generated parser is the syntactic class
      program. The type that is returned when a program is recognized is of
      Ast.ast_program list  *) 
*/

%start program
%type <unit> program

%%

/* (* Grammar rules *) */
program : pdef_list T_eof { () }
        ;

pdef_list : /* nothing */      { () }
          | letdef pdef_list   { () }
          | typedef pdef_list  { () }
          ;

letdef : T_let T_rec def def_list { () }
       | T_let def def_list { () }
       ;

def_list  : /* nothing */ { () }
          | T_and def def_list { () }
          ;

typedef : T_type tdef tdef_list { () }
        ;

tdef_list : /* nothing */ { () }
          | T_and tdef tdef_list { () }
          ;

def : T_cname par_list T_colon typ T_eq expr { () }
    | T_cname par_list T_eq expr { () }
    | T_mutable T_cname T_lbrack expr comm_list T_rbrack T_colon typ { () }
    | T_mutable T_cname T_colon typ { () }
    | T_mutable T_cname T_lbrack expr comm_list T_rbrack { () }
    | T_mutable T_cname { () }
    ;

tdef : T_cname T_eq constr constr_list { () }
     ;

par_list : /* nothing */ { () }
         | par par_list { () }
         ;

comm_list : /* nothing */ { () }
         | T_comma expr comm_list { () }
         ;

constr_list : /* nothing */ { () }
            |  T_pipe constr constr_list { () }
            ;

constr : T_constructor T_of typ typ_list { () }
       | T_constructor { () }
       ;

typ_list : /* nothing */ { () }
         | typ typ_list { () }
         ;

par : T_cname { () }
    | T_lparen T_cname T_colon typ T_rparen { () }
    ;

typ : T_array T_lbrack T_mul mul_list T_rbrack T_of typ { () }
    | T_array T_of typ { () }
    | typ T_gives typ { () }
    | T_unit { () }
    | T_int  { () }
    | T_char { () }
    | T_bool { () }
    | T_float { () }
    | T_lparen typ T_rparen { () }
    | typ T_ref { () }    
    | T_cname { () }
    ;

mul_list : /* nothing */ { () }
         | T_comma T_mul mul_list { () }
         ;

expr : letdef T_in expr { () }
     | expr T_semicolon expr { () }
     | T_if expr T_then expr T_else expr { () }
     | T_if expr T_then expr { () }
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
     | expr T_assign expr { () }
     | unary_expr { () }
     ;

unary_expr : T_plus unary_expr { () }
           | T_minus unary_expr { () }
           | T_fplus unary_expr { () }
           | T_fminus unary_expr { () }
           | T_not unary_expr { () }
           | T_delete unary_expr { () }
           | app { () }
           ;

app /* (* function call *) */
    : atom { () }
    | T_cname atom atom_list { () }
    | T_constructor atom atom_list { () }
    ;

atom_list: /* nothing */ { () }
         | atom atom_list { () }
         ;

atom /* (* un-reference *) */
     : T_bar atom { () }
     | array_el { () }
     ;

array_el /* (* array element  *) */
         : T_cname T_lbrack expr comm_list T_rbrack { () }
         | new_stmt { () }
         ;

new_stmt /* (* dynamic memory allocation *) */
         : T_new typ { () }
         | simple_expr { () }
         ;

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
           | T_match expr T_with clause clause_list T_end { () }
           /* (* parentheses and imperative structures *) */
           | T_lparen expr T_rparen { () }
           | T_begin expr T_end { () }
           | T_while expr T_do expr T_done { () }
           | T_for T_cname T_eq expr T_to expr T_do expr T_done { () }
           | T_for T_cname T_eq expr T_downto expr T_do expr T_done { () }
           /* (* simple names *) */
           | T_cname   { () }
           | T_constructor { () }
           ;

clause_list : /* nothing */ { () }
            | T_pipe clause clause_list { () }
            ;

clause : pattern T_gives expr { () }
       ;

pattern : T_constructor sp_list { () }
        | simple_pattern { () }
        ;

simple_pattern : T_plus T_intnum { () }
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
               ;

sp_list : /* nothing */ { () }
        | simple_pattern sp_list { () }
        ;

