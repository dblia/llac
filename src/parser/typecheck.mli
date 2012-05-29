(* Type checking and semantic analysis *)

(* val typeOf : Ast.ast_prog -> Types.ty ;; *)
val typeOfExpr : Ast.ast_expr -> Types.ty ;;
