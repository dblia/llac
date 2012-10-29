val lexer : Lexing.lexbuf -> Parser.token ;;
val create : string -> in_channel -> Lexing.lexbuf ;;
val add_info : Lexing.lexbuf -> Error.finfo ;;
