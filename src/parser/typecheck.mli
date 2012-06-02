(* Type checking and semantic analysis *)

val typeOf : Ast.ast_prog -> unit ;;

val new_parameter : Symbol.entry -> Ast.ast_vardef -> Symbol.entry ;;
