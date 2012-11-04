(* Symbol table *)

type pass_mode = PASS_BY_VALUE | PASS_BY_REFERENCE

type param_status =
  | PARDEF_COMPLETE                             (* Πλήρης ορισμός     *)
  | PARDEF_DEFINE                               (* Εν μέσω ορισμού    *)
  | PARDEF_CHECK                                (* Εν μέσω ελέγχου    *)

type scope = {
  sco_parent : scope option;
  sco_nesting : int;
  mutable sco_entries : entry list;
  mutable sco_negofs : int;
  mutable sco_hidden : bool
}

and variable_info = {                         (******* Μεταβλητή *******)
  variable_type   : Types.ty;                 (* Τύπος                 *)
  variable_offset : int                       (* Offset στο Ε.Δ.       *)
}

and function_info = {                         (******* Συνάρτηση *******)
  mutable function_isForward : bool;          (* Δήλωση forward        *)
  mutable function_paramlist : entry list;    (* Λίστα παραμέτρων      *)
  mutable function_redeflist : entry list;    (* Λίστα παραμέτρων (2η) *)
  mutable function_result    : Types.ty;      (* Τύπος αποτελέσματος   *)
  mutable function_pstatus   : param_status;  (* Κατάσταση παραμέτρων  *)
  mutable function_initquad  : int            (* Αρχική τετράδα        *)
}

and parameter_info = {                        (****** Παράμετρος *******)
  parameter_type           : Types.ty;        (* Τύπος                 *)
  mutable parameter_offset : int;             (* Offset στο Ε.Δ.       *)
  parameter_mode           : pass_mode        (* Τρόπος περάσματος     *)
}

and temporary_info = {                        (** Προσωρινή μεταβλητή **)
  temporary_type   : Types.ty;                (* Τύπος                 *)
  temporary_offset : int                      (* Offset στο Ε.Δ.       *)
}

and entry_info =
    ENTRY_none
  | ENTRY_variable of variable_info
  | ENTRY_function of function_info
  | ENTRY_parameter of parameter_info
  | ENTRY_temporary of temporary_info

and entry = {
  entry_id    : Identifier.id;
  entry_scope : scope;
  entry_info  : entry_info
}

type lookup_type = LOOKUP_CURRENT_SCOPE | LOOKUP_ALL_SCOPES

val start_positive_offset : int     (* Αρχικό θετικό offset στο Ε.Δ.   *)
val start_negative_offset : int     (* Αρχικό αρνητικό offset στο Ε.Δ. *)

val currentScope : scope ref              (* Τρέχουσα εμβέλεια         *)
val quadNext : int ref                    (* Αριθμός επόμενης τετράδας *)
val tempNumber : int ref                  (* Αρίθμηση των temporaries  *)

val initSymbolTable  : int -> unit
val openScope        : unit -> unit
val closeScope       : unit -> unit
val hideScope        : scope -> bool -> unit
val newVariable      : Error.finfo -> Identifier.id -> Types.ty -> bool -> entry
val newFunction      : Error.finfo -> Identifier.id -> bool -> entry
val newParameter     : Error.finfo -> Identifier.id -> Types.ty -> pass_mode ->
                                        entry -> bool -> entry
val newTemporary     : Error.finfo -> Types.ty -> entry

val forwardFunction   : entry -> unit
val endFunctionHeader : entry -> Types.ty -> unit
val lookupEntry       : Error.finfo -> Identifier.id -> lookup_type -> bool -> entry

