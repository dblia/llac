(* Type checking and semantic analysis *)

val typeOf : Ast.ast_prog -> unit ;;
val typeOfExpr : Ast.ast_expr -> Types.ty ;;
