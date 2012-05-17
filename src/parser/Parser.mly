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



/* (* The starting production of the generated parser is the syntactic class
      program. The type that is returned when a program is recognized is unit
    *) */
%start program
%type <unit> program

%%

/* (* Grammar rules *) */

