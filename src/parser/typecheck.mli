(* Type checking and semantic analysis *)

val new_parameter : Symbol.entry -> Ast.ast_vardef -> Symbol.entry;;

val typeOf : Ast.ast_prog -> unit ;;
val typeOfExpr : Ast.ast_expr -> Types.ty ;;
