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

  open Printf
  open Lexer

  type tree =
    (* Terminal Symbols  *) 
    | Unit
    | True
    | False
    | Int_Const of int
    | Float_Const of float
    | Char_Const of char
    (* Unary treeessions *)
    | Uplus of tree
    | Uminus of tree
    | UFplus of tree
    | UFminus of tree
    | Unref of tree
    | Not of tree
    (* Binary treeessions *)
    | Plus of tree * tree
    | Minus of tree * tree 
    | Times of tree * tree
    | Div of tree * tree
    | FPlus of tree * tree
    | FMinus of tree * tree 
    | FTimes of tree * tree
    | FDiv of tree * tree
    | Mod of tree * tree
    | Pow of tree * tree
    | Eq of tree * tree
    | Differ of tree * tree
    | Lt of tree * tree
    | Gt of tree * tree
    | Le of tree * tree
    | Ge of tree * tree
    | Equal of tree * tree
    | NEqual of tree * tree
    | Andlogic of tree * tree
    | Orlogic of tree * tree
    | Sequence of tree * tree
    | Assign of tree * tree

  let rec pp_tree out t = 
    match t with
      | Unit              -> fprintf out "()"
      | True              -> fprintf out "True"
      | False             -> fprintf out "False"
      | Int_Const n       -> fprintf out "%d" n
      | Float_Const n     -> fprintf out "%f" n
      | Char_Const c      -> fprintf out "%c" c
      | Uplus t1          -> fprintf out "Uplus(%a)" (pp_tree t1)
      | Uminus t1         -> fprintf out "Uminus(%a)" (pp_tree t1)
      | UFplus t1         -> fprintf out "UFplus(%a" (pp_tree t1)
      | UFminus t1        -> fprintf out "UFminus(%a)" (pp_tree t1)
      | Unref t1          -> fprintf out "Unref(%a)" (pp_tree t1) 
      | Not t1            -> fprintf out "Not(%a)" (pp_tree t1)
      | Plus (t1, t2)     -> fprintf out "Plus(%a, %a)" (pp_tree t1) (pp_tree t2)
      | Minus (t1, t2)    -> fprintf out "Minus(%a, %a)" (pp_tree t1) (pp_tree t2)
      | Times (t1, t2)    -> fprintf out "Times(%a, %a)" (pp_tree t1) (pp_tree t2)
      | Div (t1, t2)      -> fprintf out "Div(%a, %a)" (pp_tree t1) (pp_tree t2)
      | FPlus (t1, t2)    -> fprintf out "FPlus(%a, %a)" (pp_tree t1) (pp_tree t2)
      | FMinus (t1, t2)   -> fprintf out "FMinus(%a, %a)" (pp_tree t1) (pp_tree t2)
      | FTimes (t1, t2)   -> fprintf out "FTimes(%a, %a)" (pp_tree t1) (pp_tree t2)
      | FDiv (t1, t2)     -> fprintf out "FDiv(%a, %a)" (pp_tree t1) (pp_tree t2)
      | Mod (t1, t2)      -> fprintf out "Mod(%a, %a)" (pp_tree t1) (pp_tree t2) 
      | Pow (t1, t2)      -> fprintf out "Pow(%a, %a)" (pp_tree t1) (pp_tree t2)
      | Eq (t1, t2)       -> fprintf out "Eq(%a, %a)" (pp_tree t1) (pp_tree t2) 
      | Differ (t1, t2)   -> fprintf out "Differ(%a, %a)" (pp_tree t1) (pp_tree t2)
      | Lt (t1, t2)       -> fprintf out "Lt(%a, %a)" (pp_tree t1) (pp_tree t2)
      | Gt (t1, t2)       -> fprintf out "Gt(%a, %a)" (pp_tree t1) (pp_tree t2)  
      | Le (t1, t2)       -> fprintf out "Le(%a, %a)" (pp_tree t1) (pp_tree t2)
      | Ge (t1, t2)       -> fprintf out "Ge(%a, %a)" (pp_tree t1) (pp_tree t2)
      | Equal (t1, t2)    -> fprintf out "Equal(%a, %a)" (pp_tree t1) (pp_tree t2)
      | NEqual (t1, t2)   -> fprintf out "NEqual(%a, %a)" (pp_tree t1) (pp_tree t2)
      | Andlogic (t1, t2) -> fprintf out "Andlogic(%a, %a)" (pp_tree t1) (pp_tree t2)
      | Orlogic (t1, t2)  -> fprintf out "Orlogic(%a, %a)" (pp_tree t1) (pp_tree t2)
      | Assign (t1, t2)   -> fprintf out "Assign(%a, %a)" (pp_tree t1) (pp_tree t2) 

%}
   
/* (* Ocamlyacc declarations *) */
%token T_eof
%token T_err

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


%start program

%type<unit> program
%type<tree> pdef_list
%type<tree> letdef 
%type<tree> def_list
%type<tree> tdef_list
%type<tree> def
%type<tree> par_list
%type<tree> comm_list
%type<tree> typedef
%type<tree> tdef 
%type<tree> constr_list
%type<tree> constr
%type<tree> typ_list
%type<tree> par
%type<tree> typ
%type<tree> mul_list
%type<tree> expr
%type<tree> unary_expr 
%type<tree> app 
%type<tree> atom_list
%type<tree> atom 
%type<tree> array_el 
%type<tree> new_stmt 
%type<tree> simple_expr 
%type<tree> clause_list
%type<tree> clause
%type<tree> pattern
%type<tree> sp_list
 

%%

/* (* Grammar rules *) */
program : pdef_list T_eof { printf "%a\n" (pp_tree $1) }
        ;

pdef_list : /* nothing */      { () }
          | letdef pdef_list   { () }
          | typedef pdef_list  { () }
          ;

letdef : T_let T_rec def def_list { () }
       | T_let def def_list { () }
       ;

def_list : /* nothing */ { () }
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
     | expr T_plus expr { Plus ($1, $3) }
     | expr T_minus expr { Minus ($1, $3) }
     | expr T_mul expr { Times ($1, $3) }
     | expr T_div expr { Div ($1, $3) }
     | expr T_fplus expr { FPlus ($1, $3) }
     | expr T_fminus expr { FMinus ($1, $3) }
     | expr T_fmul expr { FTimes ($1, $3) }
     | expr T_fdiv expr { FDiv ($1, $3) }
     | expr T_mod expr { Mod ($1, $3) }
     | expr T_pow expr { Pow ($1, $3) }
     | expr T_eq expr { Eq ($1, $3) }
     | expr T_differ expr { Differ ($1, $3) }
     | expr T_lt expr { Lt ($1, $3) }
     | expr T_gt expr { Gt ($1, $3) }
     | expr T_le expr { Le ($1, $3) }
     | expr T_ge expr { Ge ($1, $3) }
     | expr T_equal expr { Equal ($1, $3) }
     | expr T_nequal expr { NEqual ($1, $3) }
     | expr T_andlogic expr { Andlogic ($1, $3) }
     | expr T_orlogic expr { Orlogic ($1, $3) }
     | expr T_assign expr { Assign ($1, $3) }
     | unary_expr { $1 }
     ;

unary_expr : T_plus unary_expr { Uplus $2 }
           | T_minus unary_expr { Uminus $2 }
           | T_fplus unary_expr {UFplus $2  }
           | T_fminus unary_expr { UFminus $2 }
           | T_not unary_expr { Not $2 }
           | T_delete unary_expr { Delet $2 }
           | app { $1 }
           ;

app /* (* function call *) */
    : atom { $1 }
    | T_cname atom atom_list { () }
    | T_constructor atom atom_list { () }
    ;

atom_list: /* nothing */ { () }
         | atom atom_list { () }
         ;

atom /* (* un-reference *) */
     : T_bar atom { () }
     | array_el { $1 }
     ;

array_el /* (* array element  *) */
         : T_cname T_lbrack expr comm_list T_rbrack { () }
         | new_stmt { $1 }
         ;

new_stmt /* (* dynamic memory allocation *) */
         : T_new typ { () }
         | simple_expr { $1 }
         ;

simple_expr /* (* constants *) */ 
           : T_intnum { Int_Const $1 }
           | T_floatnum { Float_Const $1 }
           | T_cchar { Char_const $1 }
           /* (*| T_string { () } *) */
           | T_true { True }
           | T_false { False }
           | T_lparen T_rparen { Unit }
           /* (* keyword-oriented *) */
           | T_dim T_intnum T_cname { () }
           | T_dim T_cname  { () }        
           | T_match expr T_with clause clause_list T_end { () }
           /* (* parentheses and imperative structures *) */
           | T_lparen expr T_rparen { $2 }
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
        | simple_pattern { $1 }
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

