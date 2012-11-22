open Ast
open InterUtils

(* Auxilary variables *)
val func_res : sem_val list ref ;;
val current_scope : string ref ;;
val mutables : (string * string) list ref ;;

(* Main Intermediate traverse function *)
val interOf : ast_prog -> unit ;;
