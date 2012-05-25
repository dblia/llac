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
  open Types
  open Ast

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
%type <Ast.ast_prog> program

%%

/* (* Grammar rules *) */
program:
    pdef T_Eof 
      { let (ldefs, tdefs) = $1 in
        PROGRAM (ldefs, tdefs) }

pdef:
  /* nothing */ 
      { ([], []) }
  | letdef program  
      { let (ldefs, tdefs) = get_name_of_prog $2 in
        ($1 :: ldefs, tdefs) }
  | typedef program 
      { let (ldefs, tdefs) = get_name_of_prog $2 in
        (ldefs, $1 :: tdefs) }

letdef:
    T_Let vardefs 
      { L_Let $2 }
  | T_Let T_Rec vardefs 
      { L_LetRec $3 }

vardefs:
    vardef 
      { [$1] }
  | vardefs T_And vardef 
      { $1 @ [$3] }

vardef:
    T_LitId formals T_Eq expr 
      { VAR_Id ($1, $2, None, $4) }
  | T_LitId formals T_Colon typee T_Eq expr 
      { VAR_Id ($1, $2, Some $4, $6) }
  | T_Mutable T_LitId 
      { VAR_MutId ($2, None, None) }
  | T_Mutable T_LitId T_Colon typee 
      { VAR_MutId ($2, Some $4, None) }
  | T_Mutable T_LitId T_LBrack expr_comma_list T_RBrack 
      { VAR_MutId ($2, None, Some $4) }
  | T_Mutable T_LitId T_LBrack expr_comma_list T_RBrack T_Colon typee 
      { VAR_MutId ($2, Some $7, Some $4) }

typedef:
    T_Type tdefs 
      { TD_Type $2 }

tdefs:
    tdef 
      { [$1] }
  | tdefs T_And tdef 
      { $1 @ [$3] }

tdef:
    T_LitId T_Eq constrs 
      { TD_TDefId ($1, $3) }

constrs:
    constr 
      { [$1] }
  | constrs T_Bar constr 
      { $1 @ [$3] }

constr:
    T_LitConstr 
      { TD_Constr ($1, None) }
  | T_LitConstr T_Of typees 
      { TD_Constr ($1, Some $3) }

formals:
    /* nothing */ 
      { [] }
  | formals param 
      { $1 @ [$2] }

param:
    T_LitId 
      { VAR_Id ($1, [], None, E_Unit) }
  | T_LParen T_LitId T_Colon typee T_RParen 
      { VAR_Id ($2, [], Some $4, E_Unit) }

typees:
    typee 
      { [$1] }
  | typees typee 
      { $1 @ [$2] }

typee:
    T_Unit  
      { TY_Unit }
  | T_Int   
      { TY_Int }
  | T_Char  
      { TY_Char }
  | T_Bool  
      { TY_Bool }
  | T_Float 
      { TY_Float }
  | T_LParen typee T_RParen 
      { $2 }
  | typee T_Gives typee 
      { TY_Function ($1, $3) }
  | typee T_Ref 
      { TY_Ref $1 }
  | T_Array T_Of typee 
      { TY_Array (1, $3) }
  | T_Array T_LBrack dimension T_RBrack T_Of typee 
      { TY_Array ($3, $6) }
  | T_LitId 
    { TY_UserDef $1 }

dimension:
    T_Mul 
      { 1 }
  | dimension T_Comma T_Mul 
      { $1 + 1 }

expr_comma_list:
    expr 
      { [$1] }
  | expr_comma_list T_Comma expr 
      { $1 @ [$3] }

expr:
  /* (* Binary Operators *) */
    expr T_Plus expr 
      { E_Plus ($1, $3) }
  | expr T_FPlus expr 
      { E_FPlus ($1, $3) }
  | expr T_Minus expr 
      { E_Minus ($1, $3) }
  | expr T_FMinus expr 
      { E_FMinus ($1, $3) }
  | expr T_Mul expr 
      { E_Mul ($1, $3) }
  | expr T_FMul expr 
      { E_FMul ($1, $3) }
  | expr T_Div expr 
      { E_Div ($1, $3) }
  | expr T_FDiv expr 
      { E_FDiv ($1, $3) }
  | expr T_Mod expr 
      { E_Mod ($1, $3) }
  | expr T_Pow expr 
      { E_Pow ($1, $3) }
  | expr T_Eq expr 
      { E_Eq ($1, $3) }
  | expr T_Differ expr 
      { E_Differ ($1, $3) }
  /* (* Relational Operators *) */
  | expr T_Equal expr 
      { E_Equal ($1, $3) }
  | expr T_NEqual expr 
      { E_NEqual ($1, $3) }
  | expr T_Lt expr 
      { E_Lt ($1, $3) }
  | expr T_Gt expr 
      { E_Gt ($1, $3) }
  | expr T_Leq expr 
      { E_Leq ($1, $3) }
  | expr T_Geq expr 
      { E_Geq ($1, $3) }
  /* (* Logical Operators *) */
  | expr T_Andlogic expr 
      { E_Andlogic ($1, $3) }
  | expr T_Orlogic expr 
      { E_Orlogic ($1, $3) }
  | expr T_Assign expr 
      { E_Assign ($1, $3) }
  | expr T_Semicolon expr 
      { E_Semicolon ($1, $3) }
  /* (* Unary Operators *) */
  | T_Plus expr %prec UPLUS 
      { E_UPlus $2 }
  | T_FPlus expr %prec UFPLUS 
      { E_UFPlus $2 }
  | T_Minus expr %prec UMINUS 
      { E_UMinus $2 }
  | T_FMinus expr %prec UFMINUS 
      { E_UFMinus $2 }
  | T_Not expr %prec NOT 
      { E_Not $2 }
  | T_Delete expr %prec DELETE 
      { E_Delete $2  }
  | letdef T_In expr 
      { E_LetIn ($1, $3) }
  /* (* If Stmt *) */
  | T_If expr T_Then expr 
      { E_IfStmt ($2, $4, None) }
  | T_If expr T_Then expr T_Else expr 
      { E_IfStmt ($2, $4, Some $6) }
  /* (*   *) */
  | T_Dim T_LitId 
      { E_Dim (None, $2) }
  | T_Dim T_LitInt T_LitId 
      { E_Dim (Some $2, $3) }
  | T_Match expr T_With clauses T_End 
      { E_Match ($2, $4) }
  /* (*   *) */
  | T_New typee 
      { E_New $2 }
  | T_Begin expr T_End 
      { E_Block $2 }
  | T_While expr T_Do expr T_Done 
      { E_While ($2, $4) }
  | T_For T_LitId T_Eq expr T_To expr T_Do expr T_Done 
      { E_For ($2, UPTO, $4, $6, $8) }
  | T_For T_LitId T_Eq expr T_Downto expr T_Do expr T_Done 
      { E_For ($2, DOWNTO, $4, $6, $8) }
    /* (* Function Call *) */
  | T_LitId exprs__ 
      { E_Call ($1, $2)  }
  | T_LitConstr exprs__ 
      { E_Constructor ($1, $2) }
  | expr__ 
      { $1 }

exprs__:
    expr__ 
      { [$1] }
  | expr__ exprs__ 
      { $1 :: $2 }

expr__:
    T_Deref expr__ 
      { E_Deref $2 }
  | T_LitId 
      { E_LitId $1 }
  | T_LitConstr 
      { E_LitConstr $1 }
  | T_True 
      { E_True }
  | T_False 
      { E_False }
  | T_LitChar 
      { E_LitChar $1 }
  | T_LitInt 
      { E_LitInt $1 }
  | T_LitFloat 
      { E_LitFloat $1 }
  | T_LitString 
      { E_LitString $1 }
  | T_LParen T_RParen 
      { E_Unit }
  | T_LParen expr T_RParen 
      { $2 }
  | T_LitId T_LBrack expr_comma_list T_RBrack /* (* array_el *) */
      { E_ArrayEl ($1, $3) }

clauses:
    clause 
      { [$1] }
  | clauses T_Bar clause 
      { $1 @ [$3] }

clause:
    pattern T_Gives expr 
      { P_Clause ($1, $3) }

patterns:
    /* nothing */ 
      { [] }
  | patterns simple_pattern 
      { $1 @ [$2] }

pattern:
    T_LitConstr patterns 
      { P_LitConstr ($1, $2) }
  | simple_pattern 
      { $1 }

simple_pattern:
    T_True 
      { P_True }
  | T_False 
      { P_False }
  | T_LitId 
      { P_LitId $1 }
  | T_LitChar 
      { P_LitChar $1 }
  | T_LitFloat 
      { P_LitFloat $1 }
  | T_Plus T_LitInt 
      { P_Plus $2 }
  | T_FPlus T_LitFloat 
      { P_FPlus $2 }
  | T_Minus T_LitInt 
      { P_Minus $2 }
  | T_FMinus T_LitFloat 
      { P_FMinus $2 }
  | T_LParen pattern T_RParen 
      { $2 }

