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
  open Types
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
      program. The type that is returned when a program is recognized is unit
    *) */
%start program
%type <Ast.ast_program> program

%%

/* (* Grammar rules *) */
program: pdef T_eof { P_prog ($1) }

pdef: /* Nothing */ { [] }
    | letdef program { let t1 = P_letdef $1 in
                          let t2 = getTypeOf $2 in t1 :: t2 }
    | typedef program { let t1 = P_typedef $1 in
                          let t2 = getTypeOf $2 in t1 :: t2 }
    ;

letdef : T_let def_list { LD_let $2  }
       | T_let T_rec def_list { LD_letrec $3 }
       ;

typedef : T_type tdef_list { TD_type $2 }
        ;

def_list  : def { [$1] }
          | def T_and def_list { $1 :: $3 }
          ;

def : T_cname par_list T_colon typ T_eq expr { DEF_invariable ($1, $2, $4, $6)  }
    | T_cname par_list T_eq expr { DEF_invariable ($1, $2, TYPE_unit, $4) }
    | T_mutable T_cname T_lbrack comm_list T_rbrack T_colon typ 
        { DEF_mutable ($2, $4, $7) }
    | T_mutable T_cname T_colon typ { DEF_mutable ($2, [], $4) }
    | T_mutable T_cname T_lbrack comm_list T_rbrack { DEF_mutable ($2, $4, TYPE_unit) }
    | T_mutable T_cname { DEF_mutable ($2, [], TYPE_unit) }
    ;

par_list : /* nothing */ { [] }
         | par par_list { $1 :: $2 }
         ;

comm_list: expr { [$1] }
         | expr T_comma comm_list { $1 :: $3 }
         ;

tdef_list : tdef { [$1] }
          | tdef T_and tdef_list { $1 :: $3 }
          ;

tdef : T_cname T_eq constr_list { TDEF_tdef ($1, $3) }
     ;

constr_list : constr { [$1] }
            | constr T_pipe constr_list { $1 :: $3 }
            ;

constr: T_constructor { C_constr ($1, []) }
      | T_constructor T_of typ_list { C_constr ($1, $3) }
      ;

par : T_cname { PAR_id ($1, TYPE_unit) }
    | T_lparen T_cname T_colon typ T_rparen { PAR_id ($2, $4) }
    ;

    typ_list : typ { [$1] }
         | typ typ_list { $1 :: $2 }
         ;

typ : T_unit { TYPE_unit }
    | T_int  { TYPE_int }
    | T_char { TYPE_float }
    | T_bool { TYPE_bool }
    | T_float { TYPE_float }
    | T_cname { TYPE_userdef $1 }
    | typ T_ref { TYPE_ref $1 }    
    | typ T_gives typ { TYPE_fun ($1, $3) }
    | T_array T_of typ { TYPE_array ($3, 1) }
    | T_lparen typ T_rparen { $2 }
    | T_array T_lbrack mul_list T_rbrack T_of typ { TYPE_array ($6, 2) }
    ;

mul_list : T_mul { () }
         | T_mul T_comma T_mul mul_list { () }
         ;

expr : /* (* binary operators *) */
     | expr T_plus expr { E_plus ($1, $3) }
     | expr T_minus expr { E_minus ($1, $3) }
     | expr T_mul expr { E_times ($1, $3) }
     | expr T_div expr { E_div ($1, $3) }
     | expr T_fplus expr { E_fplus ($1, $3) }
     | expr T_fminus expr { E_fminus ($1, $3) }
     | expr T_fmul expr { E_ftimes ($1, $3) }
     | expr T_fdiv expr { E_fdiv ($1, $3) }
     | expr T_mod expr { E_mod ($1, $3) }
     | expr T_pow expr { E_pow ($1, $3) }
     | expr T_eq expr { E_eq ($1, $3) }
     | expr T_differ expr { E_differ ($1, $3) }
     | expr T_lt expr { E_lt ($1, $3) }
     | expr T_gt expr { E_gt ($1, $3) }
     | expr T_le expr { E_le ($1, $3) }
     | expr T_ge expr { E_ge ($1, $3) }
     | expr T_equal expr { E_equal ($1, $3) }
     | expr T_nequal expr { E_nequal ($1, $3) }
     | expr T_andlogic expr { E_andlogic ($1, $3) }
     | expr T_orlogic expr { E_orlogic ($1, $3) }
     | expr T_assign expr { E_assign ($1, $3) }
     | expr T_semicolon expr { E_sequence ($1, $3) }
     | letdef T_in expr { E_letin ($1, $3)  }
     | T_if expr T_then expr { E_ifStmt ($2, $4) }
     | T_if expr T_then expr T_else expr { E_ifElse ($2, $4, $6) }
     | unary_expr { $1 }
     ;

unary_expr : T_plus unary_expr { $2 }
           | T_minus unary_expr { E_uminus $2 }
           | T_fplus unary_expr { $2 }
           | T_fminus unary_expr { E_ufminus $2 }
           | T_not unary_expr { E_not $2 }
           | T_delete unary_expr { E_delete $2 }
           | app { $1 }
           ;

app : /* (* function call *) */
    | atom { $1 }
    | T_cname atom_list { E_app ($1, $2) }
    | T_constructor atom_list { E_constr ($1, $2) }
    ;

atom_list : atom { [$1] }
          | atom atom_list { $1 :: $2 }
          ;

atom : /* (* un-reference *) */
     | T_bar atom { E_deref $2 }
     | array_el { $1 }
     ;

array_el :  /* (* array element  *) */
         | new_stmt { $1 }
         | T_cname T_lbrack comm_list T_rbrack { E_arrayEl ($1, $3) }
         ;

new_stmt : /* (* dynamic memory allocation *) */
         | T_new typ { E_new $2 }
         | simple_expr { $1 }
         ;

simple_expr:/* (* constants *) */ 
           | T_cname   { E_id $1  }
           | T_constructor { E_constr ($1, []) }
           /* (* simple names *) */
           | T_intnum { E_intConst $1 }
           | T_floatnum { E_floatConst $1 }
           | T_cchar { E_charConst $1 }
           | T_string { E_string $1 }
           | T_true { E_true }
           | T_false { E_false }
           | T_lparen T_rparen { E_unit }
           /* (* keyword-oriented *) */
           | T_dim T_intnum T_cname { E_dim ($2, $3) }
           | T_dim T_cname  { E_dim (1, $2) }        
           | T_match expr T_with clause_list T_end { E_match ($2, $4) }
           /* (* parentheses and imperative structures *) */
           | T_lparen expr T_rparen { $2 }
           | T_begin expr T_end { $2 }
           | T_while expr T_do expr T_done { E_while ($2, $4) }
           | T_for T_cname T_eq expr T_to expr T_do expr T_done 
              { E_for (TO, $2, $4, $6, $8) }
           | T_for T_cname T_eq expr T_downto expr T_do expr T_done
              { E_for (DOWNTO, $2, $4, $6, $8) }
           ;

clause_list : clause { [$1] }
            | clause T_pipe clause_list { $1 :: $3 }
            ;

clause : pattern T_gives expr { C_clause ($1, $3) }
       ;

pattern : T_constructor sp_list { PAT_constr ($1, $2) }
        | simple_pattern { $1 }
        ;

sp_list : /* nothing */ { [] }
        | simple_pattern sp_list { $1 :: $2 }
        ;

simple_pattern : T_plus T_intnum { PAT_int $2 }
               | T_minus T_intnum { PAT_int $2 }
               | T_fplus T_floatnum { PAT_float $2 }
               | T_fminus T_floatnum { PAT_float $2 }
               | T_intnum { PAT_int $1 }
               | T_floatnum { PAT_float $1 }
               | T_cchar { PAT_char $1 }
               | T_true { PAT_bool true }
               | T_false { PAT_bool false }
               | T_cname { PAT_id $1 }  
               | T_lparen pattern T_rparen { $2 }
               ;

