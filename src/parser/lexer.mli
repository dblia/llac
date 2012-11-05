(* Lexer Interface file *)

(* Main lexing rule function  *)
val lexer : Lexing.lexbuf -> Parser.token ;;
(* Open the file given for lexing process  *)
val create : string -> in_channel -> Lexing.lexbuf ;;
(* Adds file information to the given lexbuf *)
val add_info : Lexing.lexbuf -> Error.finfo ;;
