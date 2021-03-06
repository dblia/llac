open Identifier
open Error
open Types
open Format

let internal = Format.print_string

module H = Hashtbl.Make (
  struct
    type t = id
    let equal = (==)
    let hash = Hashtbl.hash
  end
)

type pass_mode = PASS_BY_VALUE | PASS_BY_REFERENCE

type param_status =
  | PARDEF_COMPLETE (* already defined function *)
  | PARDEF_DEFINE   (*   *) 
  | PARDEF_CHECK    (*   *)

type scope = {
  sco_parent : scope option;
  sco_nesting : int;
  mutable sco_entries : entry list;
  mutable sco_negofs : int;
  mutable sco_hidden : bool
}

and variable_info = {
  variable_type   : Types.ty;
  variable_offset : int
}

and function_info = {
  mutable function_isForward : bool;
  mutable function_paramlist : entry list;
  mutable function_redeflist : entry list;
  mutable function_result    : Types.ty;
  mutable function_pstatus   : param_status;
  mutable function_initquad  : int
}

and parameter_info = {
  parameter_type           : Types.ty;
  mutable parameter_offset : int;
  parameter_mode           : pass_mode
}

and temporary_info = {
  temporary_type   : Types.ty;
  temporary_offset : int
}

and entry_info = ENTRY_none
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

let start_positive_offset = 8
let start_negative_offset = 0

let the_outer_scope = {
  sco_parent = None;
  sco_nesting = 0;
  sco_entries = [];
  sco_negofs = start_negative_offset;
  sco_hidden = false
}

let no_entry id = {
  entry_id = id;
  entry_scope = the_outer_scope;
  entry_info = ENTRY_none
}

let currentScope = ref the_outer_scope
let quadNext = ref 1
let tempNumber = ref 1

(* init hash table to 0 size *)
let tab = ref (H.create 0)

let initSymbolTable size =
   tab := H.create size;
   currentScope := the_outer_scope

(* creates a new scope:
 * one level nested from the current scope *)
let openScope () =
  let sco = {
    sco_parent = Some !currentScope;
    sco_nesting = !currentScope.sco_nesting + 1;
    sco_entries = [];
    sco_negofs = start_negative_offset;
    sco_hidden = false
  } in
  currentScope := sco

(* closes current scope and sets current to it's parent *)
let closeScope () =
  let sco = !currentScope in
  let manyentry e = H.remove !tab e.entry_id in
  List.iter manyentry sco.sco_entries;
  match sco.sco_parent with
  | Some scp ->
      currentScope := scp
  | None -> (* that means that you are in the outer_scope *)
      internal "cannot close the outer scope!"

(* changes the visibility of the scope given *)
let hideScope sco flag =
  sco.sco_hidden <- flag

exception Failure_NewEntry of entry

(* adds a new entry in the current scope:
 * the type of entry depends on the inf given (see functions below).
 * set 'err' to true if you want to check for duplicate entry_id's,
 * else set 'err' to false  *)
let newEntry fi id inf err =
    if err then begin
      try
        let e = H.find !tab id in
        if e.entry_scope.sco_nesting = !currentScope.sco_nesting then ( 
          error_args fi "duplicate identifier:" id;
          raise (Exit 3))
      with Not_found ->
        ()
    end;
    let e = {
      entry_id = id;
      entry_scope = !currentScope;
      entry_info = inf
    } in
    H.add !tab id e;
    !currentScope.sco_entries <- e :: !currentScope.sco_entries;
    e

(* lookup for the entry given in the scope asked:
 * set 'err' to true or false depending on if you want to
 * print possible errors or not *)    
let lookupEntry fi id how err =
  let scc = !currentScope in
  let lookup () =
    match how with
    | LOOKUP_CURRENT_SCOPE ->
        let e = H.find !tab id in
        if e.entry_scope.sco_nesting = scc.sco_nesting then e
        else raise Not_found
    | LOOKUP_ALL_SCOPES ->
        let rec walk es =
          match es with
          | [] ->
              raise Not_found
          | e :: es ->
              if not e.entry_scope.sco_hidden then e
              else walk es
        in walk (H.find_all !tab id)
  in
  if err then
    try
      lookup ()
    with Not_found ->
      let str = "Unknown identifier (first occurence):" in
      error_args fi str id;
      (* put it in, so we don't see more errors *)
      H.add !tab id (no_entry id);
      raise (Exit 3)
  else
    lookup ()

(* adds a new variable into the current scope *)    
let newVariable fi id typ err =
  !currentScope.sco_negofs <- !currentScope.sco_negofs - sizeOfType typ;
  let inf = {
    variable_type = typ;
    variable_offset = !currentScope.sco_negofs
  } in
  newEntry fi id (ENTRY_variable inf) err

(* *)  
let newFunction fi id err =
  try
    let e = lookupEntry fi id LOOKUP_CURRENT_SCOPE false in
    match e.entry_info with
    | ENTRY_function inf when inf.function_isForward ->
        inf.function_isForward <- false;
        inf.function_pstatus <- PARDEF_CHECK;
        inf.function_redeflist <- inf.function_paramlist;
        e
    | _ ->
        if err then
          error_args fi "duplicate identifier:" id;
          raise (Exit 3)
  with Not_found ->
    let inf = {
      function_isForward = false;
      function_paramlist = [];
      function_redeflist = [];
      function_result = TY_Unit;
      function_pstatus = PARDEF_DEFINE;
      function_initquad = 0
    } in
    newEntry fi id (ENTRY_function inf) false

(* *)
let newParameter fi id typ mode f err =
  match f.entry_info with
  | ENTRY_function inf -> begin
      match inf.function_pstatus with
      | PARDEF_DEFINE ->
          let inf_p = {
            parameter_type = typ;
            parameter_offset = 0;
            parameter_mode = mode
          } in
          let e = newEntry fi id (ENTRY_parameter inf_p) err in
          inf.function_paramlist <- e :: inf.function_paramlist;
          e
      | PARDEF_CHECK -> begin
          match inf.function_redeflist with
          | p :: ps -> begin
              inf.function_redeflist <- ps;
              match p.entry_info with
              | ENTRY_parameter inf ->
                  if not (equalType inf.parameter_type typ) then
                    let str = "Parameter type mismatch in redeclaration \
                           of function:" in 
                    error_args fi str f.entry_id
                  else if inf.parameter_mode != mode then
                    let str = "Parameter passing mode mismatch in redeclaration \
                           of function:" in 
                   error_args fi str f.entry_id
                  else if p.entry_id != id then
                    let str = "Parameter name mismatch in redeclaration \
                           of function:" in
                   error_args fi str f.entry_id
                  else begin
                    H.add !tab id p;
                    !currentScope.sco_entries <- p :: !currentScope.sco_entries
                  end;
                  p
              | _ ->
                  internal "I found a parameter that is not a parameter!";
                  raise (Exit 3)
            end
          | [] ->
              let str = "More parameters than expected in redeclaration \
                     of function:" in
              error_args fi str f.entry_id;
              raise (Exit 3)
        end
      | PARDEF_COMPLETE ->
          internal "Cannot add a parameter to an already defined function";
          raise (Exit 3)
    end
  | _ ->
      internal "Cannot add a parameter to a non-function";
      raise (Exit 3)

let newTemporary fi typ =
  let id = id_make ("$" ^ string_of_int !tempNumber) in
  !currentScope.sco_negofs <- !currentScope.sco_negofs - sizeOfType typ;
  let inf = {
    temporary_type = typ;
    temporary_offset = !currentScope.sco_negofs
  } in
  incr tempNumber;
  newEntry fi id (ENTRY_temporary inf) false
;;

(* finds the forward definitions from the entry given *)
let forwardFunction e =
  match e.entry_info with
  | ENTRY_function inf ->
      inf.function_isForward <- true
  | _ ->
      internal "Cannot make a non-function forward"

let endFunctionHeader e typ =
  match e.entry_info with
  | ENTRY_function inf ->
      begin
        match inf.function_pstatus with
        | PARDEF_COMPLETE ->
            internal "Cannot end parameters in an already defined function"
        | PARDEF_DEFINE ->
            inf.function_result <- typ;
            let offset = ref start_positive_offset in
            let fix_offset e =
              match e.entry_info with
              | ENTRY_parameter inf ->
                  inf.parameter_offset <- !offset;
                  let size =
                    match inf.parameter_mode with
                    | PASS_BY_VALUE     -> sizeOfType inf.parameter_type
                    | PASS_BY_REFERENCE -> 2 in
                  offset := !offset + size
              | _ ->
                  internal "Cannot fix offset to a non parameter" in
            List.iter fix_offset inf.function_paramlist;
            inf.function_paramlist <- List.rev inf.function_paramlist
        | PARDEF_CHECK ->
            if inf.function_redeflist <> [] then
              error2 "Fewer parameters than expected in redeclaration \
                     of function %a" pretty_id e.entry_id;
            if not (equalType inf.function_result typ) then
              error2 "Result type mismatch in redeclaration of function %a"
                    pretty_id e.entry_id;
      end;
      inf.function_pstatus <- PARDEF_COMPLETE
  | _ ->
      internal "Cannot end parameters in a non-function"

(* Pretty printing. *)
open Format

let show_offsets = true

let str_of_entry_info = function
    ENTRY_none        -> "ENTRY_none"
  | ENTRY_variable _  -> "ENTRY_variable"
  | ENTRY_function _  -> "ENTRY_function"
  | ENTRY_parameter _ -> "ENTRY_parameter"
  | ENTRY_temporary _ -> "ENTRY_temporary"

let rec pretty_type ppf = function
  | TY_None ->
      fprintf ppf "<undefined>"
  | TY_Int ->
      fprintf ppf "int"
  | TY_Float ->
      fprintf ppf "float"
  | TY_Unit ->
      fprintf ppf "unit"
  | TY_Char ->
      fprintf ppf "char"
  | TY_Bool ->
      fprintf ppf "bool"
  | TY_Ref ty ->
      fprintf ppf "ref";
      pretty_type ppf ty
  | TY_Array (sz, et) ->
      pretty_type ppf et;
      if sz > 0 then
        fprintf ppf " [%d]" sz
      else
        fprintf ppf " []"
  | TY_UserDef s ->
      fprintf ppf "Userdef: %s" s
  | TY_Function (_, ty) ->
      fprintf ppf "function"

let pretty_mode ppf = function
  | PASS_BY_REFERENCE ->
      fprintf ppf "reference "
  | _ ->
      ()

let print_symbol_table () =
  let rec walk ppf scp =
    if scp.sco_nesting <> 0 then begin
      fprintf ppf "scope: ";
      let entry ppf e =
        fprintf ppf "%a" pretty_id e.entry_id;
        match e.entry_info with
        | ENTRY_none ->
            fprintf ppf "<none>"
        | ENTRY_variable inf ->
            if show_offsets then
              fprintf ppf "[%d]" inf.variable_offset
        | ENTRY_function inf ->
            let param ppf e =
              match e.entry_info with
                | ENTRY_parameter inf ->
                   fprintf ppf "%a%a : %a"
                      pretty_mode inf.parameter_mode
                      pretty_id e.entry_id
                      pretty_type inf.parameter_type
                | _ ->
                    fprintf ppf "<invalid>" in
            let rec params ppf ps =
              match ps with
              | [p] ->
                  fprintf ppf "%a" param p
              | p :: ps ->
                  fprintf ppf "%a; %a" param p params ps;
              | [] ->
                  () in
            fprintf ppf "(%a) : %a"
              params inf.function_paramlist
              pretty_type inf.function_result
        | ENTRY_parameter inf ->
            if show_offsets then
              fprintf ppf "[%d]" inf.parameter_offset
        | ENTRY_temporary inf ->
            if show_offsets then
              fprintf ppf "[%d]" inf.temporary_offset in
      let rec entries ppf es =
        match es with
          | [e] ->
              fprintf ppf "%a" entry e
          | e :: es ->
              fprintf ppf "%a, %a" entry e entries es;
          | [] ->
              () in
      match scp.sco_parent with
      | Some scpar ->
          fprintf ppf "%a\n%a"
            entries scp.sco_entries
            walk scpar
      | None ->
          fprintf ppf "<impossible>\n"
    end in
  let scope ppf scp =
    if scp.sco_nesting == 0 then
      fprintf ppf "no scope\n"
    else
      walk ppf scp in
  printf "%a----------------------------------------\n"
    scope !currentScope

