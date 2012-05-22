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

%}
  
/* (* Ocamlyacc declarations
    *
    * Identifier and Constant value Tokens *) */
%token T_Eof
%token T_Error

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
%token T_Lt T_Gt T_Leq T_Geq
%token T_Semicolon T_Comma
%token T_Andlogic T_Orlogic T_Not 
%token T_Plus T_Minus T_Mul T_Div T_Pow
%token T_FPlus T_FMinus T_FMul T_FDiv
%token T_LParen T_RParen T_LBrack T_RBrack

/* (* Precedence declarations: The lower the declaration is, the higher it's
    * precedence. *) */ 

/* (* Precedence for expressions *) */
%nonassoc T_In
%left T_Semicolon
%left T_If T_Then
%nonassoc T_Else
%right T_Assign
%left T_Orlogic
%left T_Andlogic
%nonassoc T_Eq T_Differ T_Lt T_Gt T_Leq T_Geq T_Equal T_NEqual
%left T_Plus T_FPlus T_Minus T_FMinus
%left T_Mul T_FMul T_Div T_FDiv T_Mod
%right T_Pow

%nonassoc UPLUS UFPLUS UMINUS UFMINUS NOT DELETE

/* (* Typedefs precedence *) */
%right T_Gives
%nonassoc T_Of T_Array
%left T_Ref

%nonassoc T_LParen T_RParen T_LBrack T_RBrack


/* (* The starting production of the generated parser is the syntactic class
      program. The type that is returned when a program is recognized is unit
    *) */
%start program
%type <unit> program

%%

/* (* Grammar rules *) */
program:
      pdef T_Eof { () }
    ;

pdef:
      /* nothing */ { () }
    | letdef program  { () }
    | typedef program { () }
    ;

letdef:
      T_Let vardefs { () }
    | T_Let T_Rec vardefs { () }
    ;

vardefs:
      vardef { () }
    | vardefs T_And vardef { () }
    ;

vardef:
      T_LitId formals T_Eq expr { () }
    | T_LitId formals T_Colon typee T_Eq expr { () }
    | T_Mutable T_LitId { () }
    | T_Mutable T_LitId T_Colon typee { () }
    | T_Mutable T_LitId T_LBrack expr_comma_list T_RBrack { () }
    | T_Mutable T_LitId T_LBrack expr_comma_list T_RBrack T_Colon typee { () }
    ;

typedef:
      T_Type tdefs { () }
    ;

tdefs:
      tdef { () }
    | tdefs T_And tdef { () }
    ;

tdef:
      T_LitId T_Eq constrs { () }
    ;

constrs:
      constr { () }
    | constrs T_Bar constr { () }
    ;

constr:
      T_LitConstr { () }
    | T_LitConstr T_Of typees { () }
    ;

formals:
      /* nothing */ { () }
    | formals param { () }
    ;
 
param:
      T_LitId { () }
    | T_LParen T_LitId T_Colon typee T_RParen { () }
    ;

typees:
      typee { () }
    | typees typee { () }
    ;

typee:
      T_Unit  { () }
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
      T_Mul { () }
    | dimension T_Comma T_Mul { () }
    ;

expr_comma_list:
      expr { () }
    | expr_comma_list T_Comma expr { () }
    ;

expr:
    /* (* Binary Operators *) */
      expr T_Plus expr { () }
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
    /* (* Relational Operators *) */
    | expr T_Equal expr { () }
    | expr T_NEqual expr { () }
    | expr T_Lt expr { () }
    | expr T_Gt expr { () }
    | expr T_Leq expr { () }
    | expr T_Geq expr { () }
    /* (* Logical Operators *) */
    | expr T_Andlogic expr { () }
    | expr T_Orlogic expr { () }
    | expr T_Assign expr { () }
    | expr T_Semicolon expr { () }
    /* (* Unary Operators *) */
    | T_Plus expr %prec UPLUS { () }
    | T_FPlus expr %prec UFPLUS { () }
    | T_Minus expr %prec UMINUS { () }
    | T_FMinus expr %prec UFMINUS { () }
    | T_Not expr %prec NOT { () }
    | T_Delete expr %prec DELETE { () }
    | letdef T_In expr { () }
    /* (* If Stmt *) */
    | T_If expr T_Then expr { () }
    | T_If expr T_Then expr T_Else expr { () }
    /* (*   *) */
    | T_Dim T_LitId { () }
    | T_Dim T_LitInt T_LitId { () }
    | T_Match expr T_With clauses T_End { () }
    /* (*   *) */
    | T_New typee { () }
    | T_Begin expr T_End { () }
    | T_While expr T_Do expr T_Done { () }
    | T_For T_LitId T_Eq expr T_To expr T_Do expr T_Done { () }
    | T_For T_LitId T_Eq expr T_Downto expr T_Do expr T_Done { () }
      /* (* Function Call *) */
    | T_LitId exprs__ { () }
    | T_LitConstr exprs__ { () }
    | expr__ { () }
    ;

exprs__:
      expr__ { () }
    | expr__ exprs__ { () }
    ;

expr__:
      T_Deref expr__ { () }
    | T_LitId { () }
    | T_LitConstr { () }
    | T_True { () }
    | T_False { () }
    | T_LitChar { () }
    | T_LitInt { () }
    | T_LitFloat { () }
    | T_LitString { () }
    | T_LParen T_RParen { () }
    | T_LParen expr T_RParen { () }
    | T_LitId T_LBrack expr_comma_list T_RBrack { () }  /* (* array_el *) */
    ;

clauses:
      clause { () }
    | clauses T_Bar clause { () }
    ;

clause:
      pattern T_Gives expr { () }
    ;

patterns:
      /* nothing */ { () }
    | patterns simple_pattern { () }
    ;

pattern:
      T_LitConstr patterns { () }
    | simple_pattern { () }
    ;

simple_pattern:
      T_True { () }
    | T_False { () }
    | T_LitId { () }
    | T_LitChar { () }
    | T_LitFloat { () }
    | T_Plus T_LitInt { () }
    | T_FPlus T_LitFloat { () }
    | T_Minus T_LitInt { () }
    | T_FMinus T_LitFloat { () }
    | T_LParen pattern T_RParen { () }
    ;

