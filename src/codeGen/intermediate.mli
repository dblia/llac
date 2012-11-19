open Ast
open InterUtils

(* Auxilary variables *)
val func_res : sem_val list ref ;;
val if_flag_unit : bool ref ;;

(* Main Intermediate traverse function *)
val interOf : ast_prog -> unit ;;
