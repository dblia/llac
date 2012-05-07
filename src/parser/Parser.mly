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
  open Ast

%}
   
/* (* Ocamlyacc declarations 
    *
    * Identifier and Constant value Tokens *) */
%token<int> T_intnum      
%token<char> T_cchar   
%token<float> T_floatnum
%token<string> T_cname
%token<string> T_constructor
%token<string> T_string

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
    * precedence.
    *
    * Predecence for type defs *) */
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
      Ast.ast_program list 
    *) */
%start program
%type <Ast.ast_program list> program

%%

/* (* Grammar rules *) */
program : T_eof { [] }
        | letdef program { $2 }
        | typedef program { $2 }
        ;

letdef : T_let def_list { () }
       | T_let T_rec def_list { () }
       ;

typedef : T_type tdef_list { () }
        ;

def_list  : def { () }
          | def T_and def_list { () }
          ;

def : T_cname par_list T_colon typ T_eq expr { () }
    | T_cname par_list T_eq expr { () }
    | T_mutable T_cname T_lbrack comm_list T_rbrack T_colon typ { () }
    | T_mutable T_cname T_colon typ { () }
    | T_mutable T_cname T_lbrack comm_list T_rbrack { () }
    | T_mutable T_cname { () }
    ;

par_list : /* nothing */ { () }
         | par par_list { () }
         ;

comm_list : expr { () }
          | expr T_comma comm_list { () }
          ;

tdef_list : tdef { () }
          | tdef T_and tdef_list { () }
          ;

tdef : T_cname T_eq constr_list { () }
     ;

constr_list : constr { () }
            | constr T_pipe constr_list { () }
            ;

constr : T_constructor { () }
       | T_constructor T_of typ_list { () }
       ;

par : T_cname { () }
    | T_lparen T_cname T_colon typ T_rparen { () }
    ;

typ_list : typ { () }
         | typ typ_list { () }
         ;

typ : T_unit { () }
    | T_int  { () }
    | T_char { () }
    | T_bool { () }
    | T_float { () }
    | T_cname { () }
    | typ T_ref { () }    
    | typ T_gives typ { () }
    | T_array T_of typ { () }
    | T_lparen typ T_rparen { () }
    | T_array T_lbrack mul_list T_rbrack T_of typ { () }
    ;

mul_list : T_mul { () }
         | T_mul T_comma T_mul mul_list { () }
         ;

expr : /* (* binary operators *) */
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
     | expr T_semicolon expr { () }
     | letdef T_in expr { () }
     | T_if expr T_then expr { () }
     | T_if expr T_then expr T_else expr { () }
     | unary_expr { () }
     ;

unary_expr : /* (* Unary Operators *) */
           | T_plus unary_expr { () }
           | T_minus unary_expr { () }
           | T_fplus unary_expr { () }
           | T_fminus unary_expr { () }
           | T_not unary_expr { () }
           | T_delete unary_expr { () }
           | app { () }
           ;

app : /* (* function call *) */
    | atom { () }
    | T_cname atom_list { () }
    | T_constructor atom_list { () }
    ;

atom_list : atom { () }
          | atom atom_list { () }
          ;

atom : /* (* un-reference *) */
     | T_bar atom { () }
     | array_el { () }
     ;

array_el :  /* (* array element  *) */
         | new_stmt { () }
         | T_cname T_lbrack comm_list T_rbrack { () }
         ;

new_stmt : /* (* dynamic memory allocation *) */
         | T_new typ { () }
         | simple_expr { () }
         ;

simple_expr:/* (* constants *) */ 
           | T_cname   { () }
           | T_constructor { () }
           /* (* simple names *) */
           | T_intnum { () }
           | T_floatnum { () }
           | T_cchar { () }
           | T_string { () }
           | T_true { () }
           | T_false { () }
           | T_lparen T_rparen { () }
           /* (* keyword-oriented *) */
           | T_dim T_intnum T_cname { () }
           | T_dim T_cname  { () }        
           | T_match expr T_with clause_list T_end { () }
           /* (* parentheses and imperative structures *) */
           | T_lparen expr T_rparen { () }
           | T_begin expr T_end { () }
           | T_while expr T_do expr T_done { () }
           | T_for T_cname T_eq expr T_to expr T_do expr T_done { () }
           | T_for T_cname T_eq expr T_downto expr T_do expr T_done { () }
           ;

clause_list : clause { () }
            | clause T_pipe clause_list { () }
            ;

clause : pattern T_gives expr { () }
       ;

pattern : T_constructor sp_list { () }
        | sp_list { () }
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

