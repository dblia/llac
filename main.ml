let main () =
  let input = 
    if Array.length Sys.argv > 1 then open_in Sys.argv.(1)
    else stdin 
  in
  let lexbuf = Lexing.from_channel input in
  let rec loop () = 
    let token = lexer lexbuf in 
      try
        Parser.program Lexer.lexer lexbuf;
      with Parsing.Parse_error ->
        Printf.eprintf "syntax error\n";
        exit 1
    if token <> T_eof then loop () in 
  loop ()

let _ = Printexc.print main ()
