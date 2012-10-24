open Semantics
open Lexing
open Error

let main =
  let input =
    if Array.length Sys.argv > 1 then open_in Sys.argv.(1)
    else stdin
  in
  let lexbuf = Lexing.from_channel input in
  try
    let ast = Parser.program Lexer.lexer lexbuf in
    (* Pp_ast.pp_prog ast; *)
    Semantics.typeOf ast;
    exit 0
  with Parsing.Parse_error ->
    let lbuf = lexbuf.lex_curr_p in
    error2 "Parsing error: %a" print_position (POS_Point lbuf);
    exit 1
;;
