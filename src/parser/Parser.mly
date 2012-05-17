%{
(***************************************************************
 *
 * Parser for Llama Language
 * Authors: Bliablias Dimitrios
 *
 * This file is part of Llamac project.
 *
****************************************************************)

(* Header Section *)

  open Lexer

%}
  
/* (* Ocamlyacc declarations
    *
    * Identifier and Constant value Tokens *) */
%token T_Eof

%token<int> T_LitInt
%token<char> T_LitChar  
%token<float> T_LitFloat
%token<string> T_LitId
%token<string> T_LitConstr
%token<string> T_LitString

/* (* Keyword Tokens *) */
%token T_Bool T_Char T_Int T_Float T_Unit
%token T_And T_Array T_Begin T_Delete T_Dim T_Do T_Done T_Downto T_Else 
      T_End T_False T_For T_If T_In T_Let T_Match T_Mod T_Mutable T_New 
      T_Of T_Rec T_Ref T_Then T_To T_True T_Type T_While T_With
 
/* (* Symbolic Tokens *) */
%token T_Bar
%token T_Colon
%token T_Gives
%token T_Assign T_Deref
%token T_Eq T_Differ    /* (* Structural equality [not array, func type] *) */
%token T_Equal T_NEqual /* (* Physical equality [only int, float, char] *) */
%token T_Lt T_Gt T_Le T_Ge
%token T_Semicolon T_Comma
%token T_Andlogic T_Orlogic T_Not 
%token T_Plus T_Minus T_Mul T_Div T_Pow
%token T_FPlus T_FMinus T_FMul T_FDiv
%token T_LParen T_RParen T_LBrack T_RBrack

/* (* Precedence declarations: The lower the declaration is, the higher it's
    * precedence. *) */

%right T_Gives
%nonassoc T_Of T_Array
%left T_Deref

/* (* Precedence for expressions *) */
%nonassoc T_In
%left T_Semicolon
%left T_If T_Then
%left T_Else
%right T_Assign
%left T_Orlogic
%left T_Andlogic
%nonassoc T_Eq T_Differ T_Lt T_Gt T_Le T_Ge T_Equal T_NEqual
%left T_Plus T_FPlus T_Minus T_FMinus
%left T_Mul T_FMul T_Div T_FDiv T_Mod
%nonassoc UPLUS UFPLUS UMINUS UFMINUS
%nonassoc T_Not
%left T_Pow


/* (* The starting production of the generated parser is the syntactic class
      program. The type that is returned when a program is recognized is unit
    *) */
%start program
%type <unit> program

%%

/* (* Grammar rules *) */
program:
    | pdefs T_Eof { () }
    ;

pdefs:
    | /* empty */ { () }
    | pdefs pdef  { () }
    ;
    
pdef:
    | letdef pdef  { () }
    | typedef pdef { () }
    ;

letdef:
    | T_Let vardefs { () }
    | T_Let T_Rec vardefs { () }
    ;

vardefs:
    | vardef { () }
    | vardefs T_And vardef { () }
    ;

vardef:
    | T_LitId formals T_Eq expr { () }
    | T_LitId formals T_Colon typee T_Eq expr { () }
    | T_Mutable T_LitId { () }
    | T_Mutable T_LitId T_Colon typee { () }
    | T_Mutable T_LitId T_LBrack actuals T_RBrack { () }
    | T_Mutable T_LitId T_LBrack actuals T_RBrack T_Colon typee { () }
    ;

typedef:
    | T_Type tdefs { () }
    ;

tdefs:
    | tdef { () }
    | tdefs T_And tdef { () }
    ;

tdef:
    | T_LitId T_Eq constrs { () }
    ;

constrs:
    | constr { () }
    | constrs T_Bar constr { () }
    ;

constr:
    | T_LitConstr { () }
    | T_LitConstr T_Of typees { () }
    ;

formals:
    | /* empty */ { () }
    | formals param { () }
    ;
 
param:
    | T_LitId { () }
    | T_LParen T_LitId T_Colon typee T_RParen { () }
    ;

typees:
    | typee { () }
    | typees typee { () }
    ;

typee:
    | T_Unit  { () }
    | T_Int   { () }
    | T_Char  { () }
    | T_Bool  { () }
    | T_Float { () }
    | T_LParen typee T_RParen { () }
    | typee T_Gives typee { () }
    | typee T_Ref { () }
    | T_Array T_Of typee { () }
    | T_Array T_LBrack dimension T_RBrack T_Of typee { () }
    | T_LitId { () }
    ;

dimension:
    | T_Mul { () }
    | dimension T_Comma T_Mul { () }
    ;

actuals:
    | expr { () }
    | actuals T_Comma expr { () }
    ;

exprs:
    | expr { () }
    | exprs expr { () }
    ;

simp_type:
    | T_True { () }
    | T_False { () }
    | T_LitId { () }
    | T_LitChar { () }
    ;

expr:
    | T_LitInt { () }
    | T_LitFloat { () }
    | T_LitString { () }
    | T_LParen T_RParen { () }
    | T_LParen expr T_RParen { () }
    | unop_expr { () }
    | binop_expr { () }
    | T_LitId exprs { () }
    | T_LitConstr exprs { () }
    | T_LitId T_LParen actuals T_RParen { () }
    | T_Dim T_LitInt T_LitId { () }
    | T_New typee { () }
    | T_Delete expr { () }
    | letdef T_In expr { () }
    | T_Begin expr T_End { () }
    | if_stmt { () }
    | T_While expr T_Do expr T_Done { () }
    | T_For T_LitId T_Eq expr T_To expr T_Do expr T_Done { () }
    | T_For T_LitId T_Eq expr T_Downto expr T_Do expr T_Done { () }
    | T_Match expr T_With clauses T_End { () }
    ;

clauses:
    | clause { () }
    | clauses T_Bar clause { () }
    ;

clause:
    | pattern T_Gives expr { () }
    ;

patterns:
    | pattern { () }
    | patterns pattern { () }
    ;

pattern:
    | T_Plus T_LitInt { () }
    | T_FPlus T_LitInt { () }
    | T_Minus T_LitFloat { () }
    | T_FMinus T_LitFloat { () }
    | T_LParen pattern T_RParen { () }
    | T_LitConstr patterns { () }
    | simp_type { () }
    ;

if_stmt:
    | T_If expr T_Then expr { () }
    | T_If expr T_Then expr T_Else expr { () }

unop_expr:
    | T_Plus expr %prec UPLUS { () }
    | T_FPlus expr %prec UFPLUS { () }
    | T_Minus expr %prec UMINUS { () }
    | T_FMinus expr %prec UFMINUS { () }
    | T_Deref expr { () }
    | T_Not expr { () }
    ;

binop_expr:
    | expr T_Plus expr { () }
    | expr T_FPlus expr { () }
    | expr T_Minus expr { () }
    | expr T_FMinus expr { () }
    | expr T_Mul expr { () }
    | expr T_FMul expr { () }
    | expr T_Div expr { () }
    | expr T_FDiv expr { () }
    | expr T_Mod expr { () }
    | expr T_Pow expr { () }
    | expr T_Eq expr { () }
    | expr T_Differ expr { () }
    | expr T_Lt expr { () }
    | expr T_Gt expr { () }
    | expr T_Le expr { () }
    | expr T_Ge expr { () }
    | expr T_Equal expr { () }
    | expr T_NEqual expr { () }
    | expr T_Andlogic expr { () }
    | expr T_Orlogic expr { () }
    | expr T_Assign expr { () }
    | expr T_Semicolon expr { () }
    ;

