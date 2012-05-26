open Lexing
open Format

exception Terminate

let numErrors = ref 0

type position =
    POS_Point of Lexing.position
;;

let print_position ppf pos =
  match pos with
    POS_Point lexpos ->
      fprintf ppf "@[file \"%s\", @ line %d, @ character %d:@]@ "
        lexpos.pos_fname lexpos.pos_lnum (lexpos.pos_cnum - lexpos.pos_bol)
;;

let message fmt =
  let fmt = "@[<v 2>" ^^ fmt ^^ "@]@;@?" in
  eprintf fmt
;;
