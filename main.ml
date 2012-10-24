open Semantics
open Lexing
open Pp_ast
open Error

let main =
  let input =
    if Array.length Sys.argv > 1 then open_in Sys.argv.(1)
    else stdin
  in
  let lexbuf = Lexing.from_channel input in
  begin
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with 
      pos_lnum = pos.pos_lnum;
      pos_bol = pos.pos_cnum; 
      pos_fname = "fuck!!"; }
  end;
  try
    let ast = Parser.program Lexer.lexer lexbuf in
    (*Pp_ast.pp_prog ast;*)
    Semantics.typeOf ast;
    exit 0
  with Parsing.Parse_error ->
    let lbuf = lexbuf.lex_curr_p in
    message "Parsing error: %a\n" print_position (POS_Point lbuf);
    exit 1
;;
