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
  open Ast
  open Types
  open Error
  open Symbol
  open InterUtils

  (* default entry type for initialization *)
  let dummy_entry id = {
    entry_id = Identifier.id_make id;
    entry_scope = !currentScope;
    entry_info = ENTRY_none;
  }

  (* default semantic value for initialization *)
  let dummy_sem id = {
    entry     = dummy_entry id;
    val_type  = Dummy;
    expr_type = TY_None;
    place     = Invalid;
    next      = [];
    true_     = [];
    false_    = [];
  }

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
      { L_Let (dummy_sem "", $1, $2) }
  | T_Let T_Rec vardefs
      { L_LetRec (dummy_sem "", $1, $3) }

vardefs:
    vardef
      { [$1] }
  | vardefs T_And vardef
      { $1 @ [$3] }

vardef:
    T_LitId formals T_Eq expr
      {
        let expr_ = $4 in
        if ((get_sem_expr expr_).val_type = Cond) then
          VAR_Id ({(dummy_sem $1.v) with val_type = Cond}, $1.i, $2, expr_)
        else
          VAR_Id (dummy_sem $1.v, $1.i, $2, expr_)
      }
  | T_LitId formals T_Colon typee T_Eq expr
      {
        let expr_ = $6 in
        if ((get_sem_expr expr_).val_type = Cond) then
          VAR_Id ({(dummy_sem $1.v) with expr_type = $4; val_type = Cond}, 
                   $1.i, $2, expr_)
        else
          VAR_Id ({(dummy_sem $1.v) with expr_type = $4}, $1.i, $2, expr_)
      }
  | T_Mutable T_LitId
      { VAR_MutId (dummy_sem $2.v, $2.i, None) }
  | T_Mutable T_LitId T_Colon typee
      { VAR_MutId ({(dummy_sem $2.v) with expr_type = $4}, $2.i, None) }
  | T_Mutable T_LitId T_LBrack expr_comma_list T_RBrack
      { VAR_MutId (dummy_sem $2.v, $2.i, Some $4) }
  | T_Mutable T_LitId T_LBrack expr_comma_list T_RBrack T_Colon typee
      { VAR_MutId ({(dummy_sem $2.v) with expr_type = $7}, $2.i, Some $4) }

typedef:
    T_Type tdefs
      { TD_Type (dummy_sem "", $1, $2) }

tdefs:
    tdef
      { [$1] }
  | tdefs T_And tdef
      { $1 @ [$3] }

tdef:
    T_LitId T_Eq constrs
      { TD_TDefId (dummy_sem $1.v, $1.i, $3) }

constrs:
    constr
      { [$1] }
  | constrs T_Bar constr
      { $1 @ [$3] }

constr:
    T_LitConstr
      { TD_Constr (dummy_sem $1.v, $1.i, None) }
  | T_LitConstr T_Of typees
  /* (* if i fix userdef types i should change expr_typ field to list *) */
      { TD_Constr (dummy_sem $1.v, $1.i, Some $3) }

formals:
    /* nothing */
      { [] }
  | formals param
      { $1 @ [$2] }

param:
    T_LitId
      { VAR_Id (dummy_sem $1.v, $1.i, [], E_Unit (dummy_sem "", dummyinfo)) }
  | T_LParen T_LitId T_Colon typee T_RParen
      { VAR_Id ({(dummy_sem $2.v) with expr_type = $4}, $2.i, [],
          E_Unit (dummy_sem "", dummyinfo)) }

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
      { E_Plus (dummy_sem "", $2, $1, $3) }
  | expr T_FPlus expr
      { E_FPlus (dummy_sem "", $2, $1, $3) }
  | expr T_Minus expr
      { E_Minus (dummy_sem "", $2, $1, $3) }
  | expr T_FMinus expr
      { E_FMinus (dummy_sem "", $2, $1, $3) }
  | expr T_Mul expr
      { E_Mul (dummy_sem "", $2, $1, $3) }
  | expr T_FMul expr
      { E_FMul (dummy_sem "", $2, $1, $3) }
  | expr T_Div expr
      { E_Div (dummy_sem "", $2, $1, $3) }
  | expr T_FDiv expr
      { E_FDiv (dummy_sem "", $2, $1, $3) }
  | expr T_Mod expr
      { E_Mod (dummy_sem "", $2, $1, $3) }
  | expr T_Pow expr
      { E_Pow (dummy_sem "", $2, $1, $3) }
  | expr T_Eq expr
      { E_Eq (dummy_sem "", $2, $1, $3) }
  | expr T_Differ expr
      { E_Differ (dummy_sem "", $2, $1, $3) }
  /* (* Relational Operators *) */
  | expr T_Equal expr
      { E_Equal (dummy_sem "", $2, $1, $3) }
  | expr T_NEqual expr
      { E_NEqual (dummy_sem "", $2, $1, $3) }
  | expr T_Lt expr
      { E_Lt (dummy_sem "", $2, $1, $3) }
  | expr T_Gt expr
      { E_Gt (dummy_sem "", $2, $1, $3) }
  | expr T_Leq expr
      { E_Leq (dummy_sem "", $2, $1, $3) }
  | expr T_Geq expr
      { E_Geq (dummy_sem "", $2, $1, $3) }
  /* (* Logical Operators *) */
  | expr T_Andlogic expr
      { E_Andlogic ({(dummy_sem "") with val_type = Cond}, $2, $1, $3) }
  | expr T_Orlogic expr
      { E_Orlogic ({(dummy_sem "") with val_type = Cond}, $2, $1, $3) }
  | expr T_Assign expr
      { E_Assign (dummy_sem "", $2, $1, $3) }
  | expr T_Semicolon expr
      { E_Semicolon (dummy_sem "", $2, $1, $3) }
  /* (* Unary Operators *) */
  | T_Plus expr %prec UPLUS
      { E_UPlus (dummy_sem "", $1, $2) }
  | T_FPlus expr %prec UFPLUS
      { E_UFPlus (dummy_sem "", $1, $2) }
  | T_Minus expr %prec UMINUS
      { E_UMinus (dummy_sem "", $1, $2) }
  | T_FMinus expr %prec UFMINUS
      { E_UFMinus (dummy_sem "", $1, $2) }
  | T_Not expr %prec NOT
      { E_Not ({(dummy_sem "") with val_type = Cond}, $1, $2) }
  | T_New typee
      { E_New ({(dummy_sem "") with expr_type = $2}, $1) }
  | T_Delete expr %prec DELETE
      { E_Delete (dummy_sem "", $1, $2)  }
  /* (* If Stmt *) */
  | T_If expr T_Then expr
      { E_IfStmt (dummy_sem "", $1, $2, $4, None) }
  | T_If expr T_Then expr T_Else expr
      { E_IfStmt (dummy_sem "", $1, $2, $4, Some $6) }
  /* (*   *) */
  | T_Dim T_LitId
      { E_Dim (dummy_sem $2.v, $2.i, None) }
  | T_Dim T_LitInt T_LitId
      { E_Dim (dummy_sem $3.v, $3.i, Some $2.v) }
  | T_Match expr T_With clauses T_End
      { E_Match (dummy_sem "", $1, $2, $4) }
  /* (*   *) */
  | letdef T_In expr
      { E_LetIn (dummy_sem "", $2, $1, $3) }
  | T_Begin expr T_End
      { E_Block (dummy_sem "", $1, $2) }
  | T_While expr T_Do expr T_Done
      { E_While (dummy_sem "", $1, $2, $4) }
  | T_For T_LitId T_Eq expr T_To expr T_Do expr T_Done
      { E_For (dummy_sem $2.v, $2.i, UPTO, $4, $6, $8) }
  | T_For T_LitId T_Eq expr T_Downto expr T_Do expr T_Done
      { E_For (dummy_sem $2.v, $2.i, DOWNTO, $4, $6, $8) }
    /* (* Function Call *) */
  | T_LitId exprs__
      { E_Call (dummy_sem $1.v, $1.i, $2)  }
  | T_LitConstr exprs__
      { E_ConstrCall (dummy_sem $1.v, $1.i, $2) }
  | expr__
      { $1 }

exprs__:
    expr__
      { [$1] }
  | expr__ exprs__
      { $1 :: $2 }

expr__:
    T_Deref expr__
      { E_Deref (dummy_sem "", $1, $2) }
  | T_LitId
      { E_LitId (dummy_sem $1.v, $1.i) }
  | T_LitConstr
      { E_LitConstr (dummy_sem $1.v, $1.i) }
  | T_True
      { E_True (
        {(dummy_sem "") with expr_type = TY_Bool; val_type = Rval}, $1) }
  | T_False
      { E_False (
        {(dummy_sem "") with expr_type = TY_Bool; val_type = Rval}, $1) }
  | T_LitChar
      { E_LitChar ({(dummy_sem "") with expr_type = TY_Char}, $1.i, $1.v) }
  | T_LitInt
      { E_LitInt ({(dummy_sem "") with expr_type = TY_Int}, $1.i, $1.v) }
  | T_LitFloat
      { E_LitFloat ({(dummy_sem "") with expr_type = TY_Float}, $1.i, $1.v) }
  | T_LitString
      { E_LitString (
        {(dummy_sem "") with expr_type = TY_Array(1, TY_Char)}, $1.i, $1.v) }
  | T_LParen T_RParen
      { E_Unit ({(dummy_sem "") with expr_type = TY_Unit}, dummyinfo) }
  | T_LParen expr T_RParen
      { $2 }
  | T_LitId T_LBrack expr_comma_list T_RBrack /* (* array_el *) */
      { E_ArrayEl (dummy_sem $1.v, $1.i, $3) }

clauses:
    clause
      { [$1] }
  | clauses T_Bar clause
      { $1 @ [$3] }

clause:
    pattern T_Gives expr
      { P_Clause (dummy_sem "", $2, $1, $3) }

patterns:
    /* nothing */
      { [] }
  | patterns simple_pattern
      { $1 @ [$2] }

pattern:
    T_LitConstr patterns
      { P_LitConstr (dummy_sem $1.v, $1.i, $2) }
  | simple_pattern
      { $1 }

simple_pattern:
    T_True
      { P_True ({(dummy_sem "") with expr_type = TY_Bool}, $1) }
  | T_False
      { P_False ({(dummy_sem "") with expr_type = TY_Bool}, $1) }
  | T_LitId
      { P_LitId (dummy_sem $1.v, $1.i) }
  | T_LitChar
      { P_LitChar ({(dummy_sem "") with expr_type = TY_Char}, $1.i, $1.v) }
  | T_LitFloat
      { P_LitFloat ({(dummy_sem "") with expr_type = TY_Float}, $1.i, $1.v) }
  | T_Plus T_LitInt
      { P_Plus ({(dummy_sem "") with expr_type = TY_Int}, $2.i, $2.v) }
  | T_FPlus T_LitFloat
      { P_FPlus ({(dummy_sem "") with expr_type = TY_Float}, $2.i, $2.v) }
  | T_Minus T_LitInt
      { P_Minus ({(dummy_sem "") with expr_type = TY_Int}, $2.i, $2.v) }
  | T_FMinus T_LitFloat
      { P_FPlus ({(dummy_sem "") with expr_type = TY_Float}, $2.i, $2.v) }
  | T_LParen pattern T_RParen
      { $2 }

