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
    "and"      { T_And }
  | "array"    { T_Array }
  | "begin"    { T_Begin }
  | "bool"     { T_Bool }
  | "char"     { T_Char }
  | "delete"   { T_Delete }
  | "dim"      { T_Dim }
  | "do"       { T_Do }
  | "done"     { T_Done }
  | "downto"   { T_Downto }
  | "else"     { T_Else }
  | "end"      { T_End }
  | "false"    { T_False }
  | "float"    { T_Float }
  | "for"      { T_For }
  | "if"       { T_If }
  | "in"       { T_In }
  | "int"      { T_Int }
  | "let"      { T_Let }
  | "match"    { T_Match }
  | "mod"      { T_Mod }
  | "mutable"  { T_Mutable }
  | "new"      { T_New }
  | "not"      { T_Not }
  | "of"       { T_Of }
  | "rec"      { T_Rec }
  | "ref"      { T_Ref }
  | "then"     { T_Then }
  | "to"       { T_To }
  | "true"     { T_True }
  | "type"     { T_Type }
  | "unit"     { T_Unit }
  | "while"    { T_While }
  | "with"     { T_With }
  (* Operators *)
  | "->"       { T_Gives }
  | "="        { T_Eq }
  | "|"        { T_Bar }
  | "+"        { T_Plus }
  | "-"        { T_Minus }
  | "*"        { T_Mul }
  | "/"        { T_Div }
  | "+."       { T_FPlus }
  | "-."       { T_FMinus }
  | "*."       { T_FMul }
  | "/."       { T_FDiv }
  | "**"       { T_Pow }
  | "!"        { T_Deref }
  | ";"        { T_Semicolon }
  | "&&"       { T_Andlogic }
  | "||"       { T_Orlogic }
  | "<>"       { T_Differ }
  | "<"        { T_Lt }
  | ">"        { T_Gt }
  | "<="       { T_Leq }
  | ">="       { T_Geq }
  | "=="       { T_Equal }
  | "!="       { T_NEqual }
  | ":="       { T_Assign }
  (* Separators *)
  | "("        { T_LParen }
  | ")"        { T_RParen }
  | "["        { T_LBrack }
  | "]"        { T_RBrack }
  | ","        { T_Comma }
  | ":"        { T_Colon }
  (* Digits Constants *)
  | intnum as integer  { T_LitInt (int_of_string integer) }
  | floatnum as fl     { T_LitFloat (float_of_string fl) }
  (* Names *)
  | cnames as name     { T_LitId (name) }
  | constr as con      { T_LitConstr (con) }
  (* Characters and strings *)
  | "'" chars "'"      { T_LitChar (Lexing.lexeme_char lexbuf 1) }
  | '"' str '"' as s   { T_LitString s}
  | '\n'               { incr_lineno lexbuf; lexer lexbuf }   (* newline *)
  | white              { lexer lexbuf }           (* Ignore white spaces *)
  | comm               { lexer lexbuf }              (* One line comment *)
  | "(*"               { comment 0 lexbuf }  (* Multiline comments start *)
  | eof                { T_Eof }
  | _ as err           { Printf.eprintf "Invalid character: '%c' (ascii: %d)\n"
                         err (Char.code err); T_Error }

(* inside comment *)
and comment level = parse
  | "*)"       { if level = 0 then lexer lexbuf
                 else comment (level-1) lexbuf }       (* goto the lexer rule *)
  | "(*"       { comment (level+1) lexbuf }            (* nested comment found *)
  | '\n'       { incr_lineno lexbuf; comment level lexbuf }
  | _          { comment level lexbuf }                (* skip comments *)
  | eof        { print_endline "Comments are not closed\n"; T_Eof }

