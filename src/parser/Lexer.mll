(***************************************************************
 *
 * Lexical Analyzer for Llama Language
 * Authors: Bliablias Dimitrios, Koukoutos Emmanouhl
 *
 * This file is part of Llamac project.
 *
****************************************************************)

(* Header Section *)
{
  open Lexing     (* for lex_curr_p field *)

  type token 
    = T_and 
    | T_array 
    | T_begin 
    | T_bool
    | T_char
    | T_delete
    | T_dim
    | T_do
    | T_done
    | T_downto
    | T_else 
    | T_end 
    | T_false 
    | T_float 
    | T_for 
    | T_if 
    | T_in 
    | T_int 
    | T_let 
    | T_match 
    | T_mod 
    | T_mutable 
    | T_new 
    | T_not 
    | T_of 
    | T_rec 
    | T_ref 
    | T_then 
    | T_to 
    | T_true 
    | T_type 
    | T_unit 
    | T_while 
    | T_with 
    | T_gives 
    | T_eq 
    | T_pipe 
    | T_plus 
    | T_minus 
    | T_mul 
    | T_div 
    | T_fplus
    | T_fminus 
    | T_fmul 
    | T_fdiv 
    | T_pow  
    | T_bar 
    | T_semicolon 
    | T_andlogic
    | T_orlogic
    | T_differ 
    | T_lt  
    | T_gt  
    | T_le  
    | T_ge  
    | T_equal  
    | T_nequal
    | T_assign
    | T_lparen 
    | T_rparen
    | T_lbrack 
    | T_rbrack
    | T_comma
    | T_colon
    | T_intnum        of int
    | T_floatnum      of float 
    | T_cname         of string 
    | T_constructor   of string
    | T_cchar         of char
    | T_string        of string
    | T_eof

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

  let char_of_string str = 
    if String.length str = 3 then String.get str 1
    else match String.get str 2 with
      | 'n'   ->  '\n'
      | 't'   ->  '\t'
      | 'r'   ->  '\r'
      | '0'   ->  Char.chr 0
      | '\\'  ->  '\\'
      | '\''  ->  '\''
      | '"'   ->  '"'
      | 'x'   -> let sub = String.sub str 3 2 in
                 Char.chr (int_of_string (String.concat "" (["0x"; sub])))
      | _ as err  -> err

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
let esc = xnn | ['n' 't' 'r' '0' '\\' '\'' '"']                 (* Escape chars *)
let chars = "'"( [^ ''' '"' '\\'] | "\\"esc )"'"
let str = '"'( [^ '\n' '\\' '"'] | "\\"esc )+'"'
let whiteSet = [' ' '\t' '\n' '\r']      (* White Spaces *)
let white  = whiteSet # ['\n']           (* Ignore white spaces *)
let comm   = "--" [^ '\n']+            (* One line comment *)

(* Rule Section *)
rule lexer = parse
  (* Key Words *)
    "and"      { T_and }
  | "array"    { T_array }
  | "begin"    { T_begin }
  | "bool"     { T_bool }
  | "char"     { T_char }
  | "delete"   { T_delete }
  | "dim"      { T_dim }
  | "do"       { T_do }
  | "done"     { T_done }
  | "downto"   { T_downto } 
  | "else"     { T_else }
  | "end"      { T_end }
  | "false"    { T_false }
  | "float"    { T_float }
  | "for"      { T_for }
  | "if"       { T_if }
  | "in"       { T_in }
  | "int"      { T_int }
  | "let"      { T_let }
  | "match"    { T_match }
  | "mod"      { T_mod }
  | "mutable"  { T_mutable }
  | "new"      { T_new }
  | "not"      { T_not }
  | "of"       { T_of }
  | "rec"      { T_rec }
  | "ref"      { T_ref }
  | "then"     { T_then }
  | "to"       { T_to }
  | "true"     { T_true }
  | "type"     { T_type }
  | "unit"     { T_unit }
  | "while"    { T_while }
  | "with"     { T_with }
  (* Operators *)
  | "->"       { T_gives }
  | "="        { T_eq }
  | "|"        { T_pipe }
  | "+"        { T_plus }
  | "-"        { T_minus }
  | "*"        { T_mul }
  | "/"        { T_div }
  | "+."       { T_fplus }
  | "-."       { T_fminus }
  | "*."       { T_fmul }
  | "/."       { T_fdiv }
  | "**"       { T_pow } 
  | "!"        { T_bar }
  | ";"        { T_semicolon }
  | "&&"       { T_andlogic }
  | "||"       { T_orlogic }
  | "<>"       { T_differ } 
  | "<"        { T_lt } 
  | ">"        { T_gt } 
  | "<="       { T_le } 
  | ">="       { T_ge } 
  | "=="       { T_equal } 
  | "!="       { T_nequal }
  | ":="       { T_assign }
  (* Separators *)
  | "("        { T_lparen }
  | ")"        { T_rparen }
  | "["        { T_lbrack } 
  | "]"        { T_rbrack }
  | ","        { T_comma }
  | ":"        { T_colon }
  (* Digits Constants *)
  | intnum as integer  { T_intnum (int_of_string integer) }
  | floatnum as fl     { T_floatnum (float_of_string fl) }
  (* Names *)
  | cnames as name     { T_cname (name) }
  | constr as con      { T_constructor (con) }
  | chars as c { T_cchar (char_of_string c) }
  | str as s   { T_string s }                          (* Strings *)
  | '\n'       { incr_lineno lexbuf; lexer lexbuf }    (* newline *)
  | white      { lexer lexbuf }                        (* Ignore white spaces *)
  | comm       { lexer lexbuf }                        (* One line comment *)
  | "(*"       { comment 0 lexbuf }                    (* Multiline comments start *)
  | eof        { T_eof }                        
  | _ as err   { Printf.eprintf "Invalid character: '%c' (ascii: %d)\n" err 
                  (Char.code err); lexer lexbuf; }
and comment level = parse
  | "*)"       { if level = 0 then lexer lexbuf
                 else comment (level-1) lexbuf }       (* goto the lexer rule *)
  | "(*"       { comment (level+1) lexbuf }            (* nested comment found *)
  | _          { comment level lexbuf }                (* skip comments *)
  | eof        { print_endline "Comments are not closed\n"; T_eof }


(* Trailer Section *)
{
let main =
  let input = 
    if Array.length Sys.argv > 1 then open_in Sys.argv.(1)
    else stdin 
  in
  let lexbuf = Lexing.from_channel input in
  let rec loop () = 
    let token = lexer lexbuf in 
      Printf.printf "lexeme: \"%s\", lineno: %d\n" (Lexing.lexeme lexbuf)
      (lexbuf.lex_curr_p.pos_lnum);
    if token <> T_eof then loop () in 
  loop ()
}
