let main =
  let input = 
    if Array.length Sys.argv > 1 then open_in Sys.argv.(1)
    else stdin 
  in
  let lexbuf = Lexing.from_channel input in
  try
    Pervasives.ignore (Parser.program Lexer.lexer lexbuf);
    exit 0
  with Parsing.Parse_error ->
    Printf.eprintf "syntax error in line %d, char %d\n" (lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum) 
        (lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum - lexbuf.Lexing.lex_curr_p.Lexing.pos_bol) ;
    exit 1
