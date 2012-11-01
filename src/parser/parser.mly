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
  open Error
  open Ast

%}

/* (* Ocamlyacc declarations
    *
    * Identifier and Constant value Tokens *) */
%token T_Error
%token<Error.finfo> T_Eof

/* (* Idetifiers and constant value tokens *) */
%token<int Error.withinfo> T_LitInt
%token<char Error.withinfo> T_LitChar
%token<float Error.withinfo> T_LitFloat
%token<string Error.withinfo> T_LitId
%token<string Error.withinfo> T_LitConstr
%token<string Error.withinfo> T_LitString

/* (* Keyword Tokens *) */
%token<Error.finfo> T_And
%token<Error.finfo> T_Bool
%token<Error.finfo> T_Char
%token<Error.finfo> T_Int
%token<Error.finfo> T_Float
%token<Error.finfo> T_Unit
%token<Error.finfo> T_Array
%token<Error.finfo> T_Begin
%token<Error.finfo> T_Delete
%token<Error.finfo> T_Dim
%token<Error.finfo> T_Do
%token<Error.finfo> T_Done
%token<Error.finfo> T_Downto
%token<Error.finfo> T_Else
%token<Error.finfo> T_End
%token<Error.finfo> T_False
%token<Error.finfo> T_For
%token<Error.finfo> T_If
%token<Error.finfo> T_In
%token<Error.finfo> T_Let
%token<Error.finfo> T_Match
%token<Error.finfo> T_Mod
%token<Error.finfo> T_Mutable
%token<Error.finfo> T_New
%token<Error.finfo> T_Of
%token<Error.finfo> T_Rec
%token<Error.finfo> T_Ref
%token<Error.finfo> T_Then
%token<Error.finfo> T_To
%token<Error.finfo> T_True
%token<Error.finfo> T_Type
%token<Error.finfo> T_While
%token<Error.finfo> T_With

/* (* Symbolic Tokens *) */
%token<Error.finfo> T_Bar
%token<Error.finfo> T_Colon
%token<Error.finfo> T_Gives
%token<Error.finfo> T_Assign
%token<Error.finfo> T_Deref
%token<Error.finfo> T_Eq     /* (* Structural equal [not array, fun type] *) */
%token<Error.finfo> T_Differ /* (* Structural equal [not array, fun type] *) */
%token<Error.finfo> T_Equal  /* (* Physical equal [only int, float, char] *) */
%token<Error.finfo> T_NEqual /* (* Physical equal [only int, float, char] *) */
%token<Error.finfo> T_Lt
%token<Error.finfo> T_Gt
%token<Error.finfo> T_Leq
%token<Error.finfo> T_Geq
%token<Error.finfo> T_Semicolon
%token<Error.finfo> T_Comma
%token<Error.finfo> T_Andlogic
%token<Error.finfo> T_Orlogic
%token<Error.finfo> T_Not
%token<Error.finfo> T_Plus
%token<Error.finfo> T_Minus
%token<Error.finfo> T_Mul
%token<Error.finfo> T_Div
%token<Error.finfo> T_Pow
%token<Error.finfo> T_FPlus
%token<Error.finfo> T_FMinus
%token<Error.finfo> T_FMul
%token<Error.finfo> T_FDiv
%token<Error.finfo> T_LParen
%token<Error.finfo> T_RParen
%token<Error.finfo> T_LBrack
%token<Error.finfo> T_RBrack

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
      { L_Let ($1, $2) }
  | T_Let T_Rec vardefs
      { L_LetRec ($1, $3) }

vardefs:
    vardef
      { [$1] }
  | vardefs T_And vardef
      { $1 @ [$3] }

vardef:
    T_LitId formals T_Eq expr
      { VAR_Id ($1.i, $1.v, $2, None, $4) }
  | T_LitId formals T_Colon typee T_Eq expr
      { VAR_Id ($1.i, $1.v, $2, Some $4, $6) }
  | T_Mutable T_LitId
      { VAR_MutId ($2.i, $2.v, None, None) }
  | T_Mutable T_LitId T_Colon typee
      { VAR_MutId ($2.i, $2.v, Some $4, None) }
  | T_Mutable T_LitId T_LBrack expr_comma_list T_RBrack
      { VAR_MutId ($2.i, $2.v, None, Some $4) }
  | T_Mutable T_LitId T_LBrack expr_comma_list T_RBrack T_Colon typee
      { VAR_MutId ($2.i, $2.v, Some $7, Some $4) }

typedef:
    T_Type tdefs
      { TD_Type ($1, $2) }

tdefs:
    tdef
      { [$1] }
  | tdefs T_And tdef
      { $1 @ [$3] }

tdef:
    T_LitId T_Eq constrs
      { TD_TDefId ($1.i, $1.v, $3) }

constrs:
    constr
      { [$1] }
  | constrs T_Bar constr
      { $1 @ [$3] }

constr:
    T_LitConstr
      { TD_Constr ($1.i, $1.v, None) }
  | T_LitConstr T_Of typees
      { TD_Constr ($1.i, $1.v, Some $3) }

formals:
    /* nothing */
      { [] }
  | formals param
      { $1 @ [$2] }

param:
    T_LitId
      { VAR_Id ($1.i, $1.v, [], None, E_Unit dummyinfo) }
  | T_LParen T_LitId T_Colon typee T_RParen
      { VAR_Id ($2.i, $2.v, [], Some $4, E_Unit dummyinfo) }

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
  | typee T_Gives typee { 
      match $3 with
      | TY_Function (lst, type_) -> TY_Function ($1 :: lst, type_)
      | _ as type_ -> TY_Function ([$1], type_)
    }
  | typee T_Ref
      { TY_Ref $1 }
  | T_Array T_Of typee
      { TY_Array (1, $3) }
  | T_Array T_LBrack dimension T_RBrack T_Of typee
      { TY_Array ($3, $6) }
  | T_LitId
    { TY_UserDef $1.v }

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
      { E_Plus ($2, $1, $3) }
  | expr T_FPlus expr
      { E_FPlus ($2, $1, $3) }
  | expr T_Minus expr
      { E_Minus ($2, $1, $3) }
  | expr T_FMinus expr
      { E_FMinus ($2, $1, $3) }
  | expr T_Mul expr
      { E_Mul ($2, $1, $3) }
  | expr T_FMul expr
      { E_FMul ($2, $1, $3) }
  | expr T_Div expr
      { E_Div ($2, $1, $3) }
  | expr T_FDiv expr
      { E_FDiv ($2, $1, $3) }
  | expr T_Mod expr
      { E_Mod ($2, $1, $3) }
  | expr T_Pow expr
      { E_Pow ($2, $1, $3) }
  | expr T_Eq expr
      { E_Eq ($2, $1, $3) }
  | expr T_Differ expr
      { E_Differ ($2, $1, $3) }
  /* (* Relational Operators *) */
  | expr T_Equal expr
      { E_Equal ($2, $1, $3) }
  | expr T_NEqual expr
      { E_NEqual ($2, $1, $3) }
  | expr T_Lt expr
      { E_Lt ($2, $1, $3) }
  | expr T_Gt expr
      { E_Gt ($2, $1, $3) }
  | expr T_Leq expr
      { E_Leq ($2, $1, $3) }
  | expr T_Geq expr
      { E_Geq ($2, $1, $3) }
  /* (* Logical Operators *) */
  | expr T_Andlogic expr
      { E_Andlogic ($2, $1, $3) }
  | expr T_Orlogic expr
      { E_Orlogic ($2, $1, $3) }
  | expr T_Assign expr
      { E_Assign ($2, $1, $3) }
  | expr T_Semicolon expr
      { E_Semicolon ($2, $1, $3) }
  /* (* Unary Operators *) */
  | T_Plus expr %prec UPLUS
      { E_UPlus ($1, $2) }
  | T_FPlus expr %prec UFPLUS
      { E_UFPlus ($1, $2) }
  | T_Minus expr %prec UMINUS
      { E_UMinus ($1, $2) }
  | T_FMinus expr %prec UFMINUS
      { E_UFMinus ($1, $2) }
  | T_Not expr %prec NOT
      { E_Not ($1, $2) }
  | T_New typee
      { E_New ($1, $2) }
  | T_Delete expr %prec DELETE
      { E_Delete ($1, $2)  }
  /* (* If Stmt *) */
  | T_If expr T_Then expr
      { E_IfStmt ($1, $2, $4, None) }
  | T_If expr T_Then expr T_Else expr
      { E_IfStmt ($1, $2, $4, Some $6) }
  /* (*   *) */
  | T_Dim T_LitId
      { E_Dim ($2.i, None, $2.v) }
  | T_Dim T_LitInt T_LitId
      { E_Dim ($3.i, Some $2.v, $3.v) }
  | T_Match expr T_With clauses T_End
      { E_Match ($1, $2, $4) }
  /* (*   *) */
  | letdef T_In expr
      { E_LetIn ($2, $1, $3) }
  | T_Begin expr T_End
      { E_Block ($1, $2) }
  | T_While expr T_Do expr T_Done
      { E_While ($1, $2, $4) }
  | T_For T_LitId T_Eq expr T_To expr T_Do expr T_Done
      { E_For ($2.i, $2.v, UPTO, $4, $6, $8) }
  | T_For T_LitId T_Eq expr T_Downto expr T_Do expr T_Done
      { E_For ($2.i, $2.v, DOWNTO, $4, $6, $8) }
    /* (* Function Call *) */
  | T_LitId exprs__
      { E_Call ($1.i, $1.v, $2)  }
  | T_LitConstr exprs__
      { E_Constructor ($1.i, $1.v, $2) }
  | expr__
      { $1 }

exprs__:
    expr__
      { [$1] }
  | expr__ exprs__
      { $1 :: $2 }

expr__:
    T_Deref expr__
      { E_Deref ($1, $2) }
  | T_LitId
      { E_LitId ($1.i, $1.v) }
  | T_LitConstr
      { E_LitConstr ($1.i, $1.v) }
  | T_True
      { E_True $1 }
  | T_False
      { E_False $1 }
  | T_LitChar
      { E_LitChar ($1.i, $1.v) }
  | T_LitInt
      { E_LitInt ($1.i, $1.v) }
  | T_LitFloat
      { E_LitFloat ($1.i, $1.v) }
  | T_LitString
      { E_LitString ($1.i, $1.v) }
  | T_LParen T_RParen
      { E_Unit dummyinfo }
  | T_LParen expr T_RParen
      { $2 }
  | T_LitId T_LBrack expr_comma_list T_RBrack /* (* array_el *) */
      { E_ArrayEl ($1.i, $1.v, $3, List.length $3) }

clauses:
    clause
      { [$1] }
  | clauses T_Bar clause
      { $1 @ [$3] }

clause:
    pattern T_Gives expr
      { P_Clause ($2, $1, $3) }

patterns:
    /* nothing */
      { [] }
  | patterns simple_pattern
      { $1 @ [$2] }

pattern:
    T_LitConstr patterns
      { P_LitConstr ($1.i, $1.v, $2) }
  | simple_pattern
      { $1 }

simple_pattern:
    T_True
      { P_True $1 }
  | T_False
      { P_False $1 }
  | T_LitId
      { P_LitId ($1.i, $1.v) }
  | T_LitChar
      { P_LitChar ($1.i, $1.v) }
  | T_LitFloat
      { P_LitFloat ($1.i, $1.v) }
  | T_Plus T_LitInt
      { P_Plus ($2.i, $2.v) }
  | T_FPlus T_LitFloat
      { P_FPlus ($2.i, $2.v) }
  | T_Minus T_LitInt
      { P_Minus ($2.i, $2.v) }
  | T_FMinus T_LitFloat
      { P_FMinus ($2.i, $2.v) }
  | T_LParen pattern T_RParen
      { $2 }

