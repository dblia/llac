open Semantics
open Lexing
open Error

let searchpath = ref [""]

let argDefs = [
  "-I",
      Arg.String (fun f -> searchpath := f::!searchpath),
      "Append a directory to the search path"]

let parseArgs () =
  let inFile = ref (None : string option) in
  let anon_fun = fun s ->
       match !inFile with
         Some(_) -> err "You must specify exactly one input file"
       | None -> inFile := Some(s)
  in
  Arg.parse argDefs anon_fun "";
  match !inFile with
      None -> err "You must specify an input file"
    | Some(s) -> s

let openfile infile = 
  let rec trynext l = match l with
        [] -> err ("Could not find " ^ infile)
      | (d::rest) -> 
          let name = if d = "" then infile else (d ^ "/" ^ infile) in
          try open_in name
            with Sys_error m -> trynext rest
  in trynext !searchpath

let main = 
  let inFile = parseArgs() in
  let pi = openfile inFile in
  let lexbuf = Lexer.create inFile pi in 
  try
    let ast = Parser.program Lexer.lexer lexbuf in
    (* Pp_ast.pp_prog ast; *)
    Semantics.typeOf ast;
    exit 0
  with Parsing.Parse_error ->
    error (Lexer.add_info lexbuf) "Parsing error";
;;

