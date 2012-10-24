(***************************************************************
 *
 * Lexical Analyzer for Llama Language
 * Authors: Bliablias Dimitrios
 *
 * This file is part of Llamac project.
 *
****************************************************************)

(* Header Section *)
{
  open Lexing
  open Parser
  open Printf
  open Error

  (* Update the line number (pos_lnum field) and the offset of the line beggining,
   * (pos_bol field). Field pos_cnum managed by the lexing engine.
   *
   * Print (lexbuf.lex_curr_p.pos_cnum-lexbuf.lex_curr_p.pos_bol) for the relative
   * offset in current line.
   *)
  let incr_lineno lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;                 (* increase newline counter *)
      pos_bol = pos.pos_cnum;       (* update the offset of the line beginning *)
    }

  let filename = ref ""

  let add_info lexbuf =
    let pos = lexbuf.lex_curr_p in
    createInfo (!filename) (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol)
}

(* Definition Section *)
let digit = ['0'-'9']                 (* Digits *)
let intnum = digit+                   (* Integer Constants *)
let floatnum = intnum"."intnum(("E"|"e")(("+"|"-")?)intnum)?  (* Float constants *)
let llow = ['a'-'z']
let lbig = ['A'-'Z']
let letter = ['a'-'z' 'A'-'Z']        (* Letters *)
let cnames = llow(letter|digit|"_")*  (* Constant names (vars, functions, types) *)
let constr = lbig(letter|digit|"_")*  (* Cunstructor names *)
let hex = digit | ['a'-'f' 'A'-'F']   (* Hex numbers *)
let xnn = "x" hex hex                 (* Char with nn ASCII code in hexademical *)
let esc = xnn | ['n' 't' 'r' '0' '\\' '\'' '\"']                  (* Escape chars *)
let chars = ( [^ '\'' '"' '\\' '\t' '\n' '\r' '\x00'] | "\\"esc ) (* fixed *)
let str = ([^ '"' '\n'] | "\\"esc)+

let whiteSet = [' ' '\t' '\n' '\r']      (* White Spaces *)
let white  = whiteSet # ['\n']           (* Ignore white spaces *)
let comm   = "--" [^ '\n']+            (* One line comment *)

(* Rule Section *)
rule lexer = parse
  (* Key Words *)
    "and"      { T_And (add_info lexbuf) }
  | "array"    { T_Array (add_info lexbuf) }
  | "begin"    { T_Begin (add_info lexbuf) }
  | "bool"     { T_Bool (add_info lexbuf) }
  | "char"     { T_Char (add_info lexbuf) }
  | "delete"   { T_Delete (add_info lexbuf) }
  | "dim"      { T_Dim (add_info lexbuf) }
  | "do"       { T_Do (add_info lexbuf) }
  | "done"     { T_Done (add_info lexbuf) }
  | "downto"   { T_Downto (add_info lexbuf) }
  | "else"     { T_Else (add_info lexbuf) }
  | "end"      { T_End (add_info lexbuf) }
  | "false"    { T_False (add_info lexbuf) }
  | "float"    { T_Float (add_info lexbuf) }
  | "for"      { T_For (add_info lexbuf) }
  | "if"       { T_If (add_info lexbuf) }
  | "in"       { T_In (add_info lexbuf) }
  | "int"      { T_Int (add_info lexbuf) }
  | "let"      { T_Let (add_info lexbuf) }
  | "match"    { T_Match (add_info lexbuf) }
  | "mod"      { T_Mod (add_info lexbuf) }
  | "mutable"  { T_Mutable (add_info lexbuf) }
  | "new"      { T_New (add_info lexbuf) }
  | "not"      { T_Not (add_info lexbuf) }
  | "of"       { T_Of (add_info lexbuf) }
  | "rec"      { T_Rec (add_info lexbuf) }
  | "ref"      { T_Ref (add_info lexbuf) }
  | "then"     { T_Then (add_info lexbuf) }
  | "to"       { T_To (add_info lexbuf) }
  | "true"     { T_True (add_info lexbuf) }
  | "type"     { T_Type (add_info lexbuf) }
  | "unit"     { T_Unit (add_info lexbuf) }
  | "while"    { T_While(add_info lexbuf)  }
  | "with"     { T_With (add_info lexbuf) }
  (* Operators *)
  | "->"       { T_Gives (add_info lexbuf) }
  | "="        { T_Eq (add_info lexbuf) }
  | "|"        { T_Bar (add_info lexbuf) }
  | "+"        { T_Plus (add_info lexbuf) }
  | "-"        { T_Minus (add_info lexbuf) }
  | "*"        { T_Mul (add_info lexbuf) }
  | "/"        { T_Div (add_info lexbuf) }
  | "+."       { T_FPlus (add_info lexbuf) }
  | "-."       { T_FMinus (add_info lexbuf) }
  | "*."       { T_FMul (add_info lexbuf) }
  | "/."       { T_FDiv (add_info lexbuf) }
  | "**"       { T_Pow (add_info lexbuf) }
  | "!"        { T_Deref (add_info lexbuf) }
  | ";"        { T_Semicolon (add_info lexbuf) }
  | "&&"       { T_Andlogic (add_info lexbuf) }
  | "||"       { T_Orlogic (add_info lexbuf) }
  | "<>"       { T_Differ (add_info lexbuf) }
  | "<"        { T_Lt (add_info lexbuf) }
  | ">"        { T_Gt (add_info lexbuf) }
  | "<="       { T_Leq (add_info lexbuf) }
  | ">="       { T_Geq (add_info lexbuf) }
  | "=="       { T_Equal (add_info lexbuf) }
  | "!="       { T_NEqual (add_info lexbuf) }
  | ":="       { T_Assign (add_info lexbuf) }
  (* Separators *)
  | "("        { T_LParen (add_info lexbuf) }
  | ")"        { T_RParen (add_info lexbuf) }
  | "["        { T_LBrack (add_info lexbuf) }
  | "]"        { T_RBrack (add_info lexbuf) }
  | ","        { T_Comma (add_info lexbuf) }
  | ":"        { T_Colon (add_info lexbuf) }
  (* Digits Constants *)
  | intnum as integer
    { T_LitInt {i = add_info lexbuf; v = int_of_string integer} }
  | floatnum as fl
    { T_LitFloat {i = add_info lexbuf; v = float_of_string fl} }
  (* Names *)
  | cnames as name     { T_LitId {i = add_info lexbuf; v = name} }
  | constr as con      { T_LitConstr {i = add_info lexbuf; v = con} }
  (* Characters and strings *)
  | "'" chars "'"
    { T_LitChar {i = add_info lexbuf; v = Lexing.lexeme_char lexbuf 1} }
  | '"' str '"'
    { T_LitChar {i = add_info lexbuf; v = Lexing.lexeme_char lexbuf 1} }
  | '\n'               { incr_lineno lexbuf; lexer lexbuf }   (* newline *)
  | white              { lexer lexbuf }           (* Ignore white spaces *)
  | comm               { lexer lexbuf }              (* One line comment *)
  | "(*"               { comment 0 lexbuf }  (* Multiline comments start *)
  | eof                { T_Eof (add_info lexbuf) }
  | _ as err           { Printf.printf "Invalid character: '%c' (ascii: %d)\n"
                         err (Char.code err); T_Error }

(* inside comment *)
and comment level = parse
  | "*)"       { if level = 0 then lexer lexbuf
                 else comment (level-1) lexbuf }    (* goto the lexer rule *)
  | "(*"       { comment (level+1) lexbuf }        (* nested comment found *)
  | '\n'       { incr_lineno lexbuf; comment level lexbuf }
  | _          { comment level lexbuf }             (* skip comments *)
  | eof        { error (dummyinfo) "Comments are not closed properly" }

