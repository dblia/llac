(* Type checking and semantic analysis *)

open Pervasives

module P = Pervasives

open Identifier
open Symbol
open Error
open Types
open Ast

exception Terminate

(* External functions that form the Llama's standard library *)
let library_functions = [
  (* Input/Output *)
  ( "print_int",    [TY_Int  ], TY_Unit );
  ( "print_float",  [TY_Float], TY_Unit );
  ( "print_bool",   [TY_Bool ], TY_Unit );
  ( "print_char",   [TY_Char ], TY_Unit );
  ( "print_string", [TY_Array (1, TY_Char)], TY_Unit );
  ( "read_int",     [TY_Unit], TY_Int) ;
  ( "read_bool",    [TY_Unit], TY_Int) ;
  ( "read_char",    [TY_Unit], TY_Char) ;
  ( "read_float",   [TY_Unit], TY_Float) ;
  ( "read_string",  [TY_Array (1, TY_Char)], TY_Int) ;
  (* Mathematical *)
  ( "abs" , [TY_Int  ], TY_Int  );
  ( "fabs", [TY_Float], TY_Float);
  ( "sqrt", [TY_Float], TY_Float);
  ( "sin" , [TY_Float], TY_Float);
  ( "cos" , [TY_Float], TY_Float);
  ( "tan" , [TY_Float], TY_Float);
  ( "atan", [TY_Float], TY_Float);
  ( "exp" , [TY_Float], TY_Float);
  ( "ln"  , [TY_Float], TY_Float);
  ( "pi"  , [TY_Unit ], TY_Float);
  (* ++/-- *)
  ( "incr", [TY_Ref TY_Int], TY_Unit );
  ( "decr", [TY_Ref TY_Int], TY_Unit );
  (* Conversions *)
  ( "float_of_int", [TY_Int  ], TY_Float );
  ( "int_of_float", [TY_Float], TY_Int   );
  ( "round",        [TY_Float], TY_Int   );
  ( "int_of_char",  [TY_Char ], TY_Int   );
  ( "char_of_int",  [TY_Int  ], TY_Char  );
  (* String Handling *)
  ( "strlen", [TY_Array (1, TY_Char)], TY_Int );
  ( "strcmp", [TY_Array (1, TY_Char); TY_Array (1, TY_Char)], TY_Int);
  ( "strcpy", [TY_Array (1, TY_Char); TY_Array (1, TY_Char)], TY_Unit);
  ( "strcat", [TY_Array (1, TY_Char); TY_Array (1, TY_Char)], TY_Unit)
]

(* auxilary variable used for the uniqueness of the standard's library 
 * parameters *)
let cnt = ref 0

(* auxilary function that insters Standard Library funtions in the Main's scope *)
let function_create (id, args, typ) =
  let fn =
    try newFunction dummyinfo (id_make id) true
    with Exit _ -> raise Terminate
  in
  openScope(); (* new scope for the args and body definition *)
  (* now we add the parameters of the function *)
  List.iter (fun typee -> P.ignore (newParameter dummyinfo (id_make ("param" ^
    (string_of_int !cnt))) typee PASS_BY_VALUE fn true); cnt := !cnt + 1) args;
  endFunctionHeader fn typ; (* end of function header *)
  closeScope() (* close the body's scope *)

(* auxilary function for adding new parameters to the scope given *)
let new_parameter f = function
    VAR_Id (fi, s, [], t, e) ->
      begin
        match t with
        | None -> error fi 3 "No typeinfer you must provide a type\n"
        | Some _t -> newParameter fi (id_make s) _t PASS_BY_VALUE f true
      end
  | _ -> (* FIXME: currying (function parameters).
          * eg. VAR_Id (s, lst, t, e) *)
      err "Not a valid parameter form\n"
;;

let rec typeOf = function
    PROGRAM (ldfs, tdfs) ->
      (* first add standard library to the Main's scope *)
      List.iter function_create library_functions;
      List.iter (fun x -> typeOfLetdef x) ldfs;
      List.iter (fun x -> typeOfTypedef x) tdfs

(* Every letfed block can re-define variables or functions that have been
 * defined in a previous letdef block.
 *
 * So it isn't actually necessary to make a new scope everytime we found a new
 * letdef but we could add the new definiton into the same scope (eg the top
 * level scope). In case of duplicate values we should add the new entry to the
 * symbol table and remove the old one.
 *
 * Nevertheless, we follow a different strategy because of the symbol's table
 * form given (we don't want to change it). Currently, we open a new scope in
 * every letdef found because later we'll want to hide the definition from the
 * body and that won't be possible because the hiddeScope func given hides the
 * entire scope and not the name which we would only like to hide. *)
and typeOfLetdef = function
  (* let:
   *
   * In a let block the variable/function defined should NOT be visible by the
   * body of the block. So we have to make it hidden by the function body.
   * rec_flag := false *)
    L_Let (fi, vl)    -> (* vl: vardefs connected with 'and' keyword *)
      openScope(); (* scope for the definition only *)
      List.iter (fun x -> typeOfVardef false x) vl
  (* let rec:
   *
   * We should make two passes to let rec definitions. The first one is for
   * adding to the symbol table all forward functions and it's parameters, and
   * in the second one we add all the definitions and we handle the body of
   * each definition.
   *
   * In a let rec block the variable/function defined should BE visible by the
   * body of the block. So we do NOT make it hidden from the function body.
   * rec_flag := true *)
  | L_LetRec (fi, vl) -> (* vl: vardefs connected with 'and' keyword *)
      openScope(); (* scope for the definition only *)
      let find_forwards = function
          VAR_Id (fi, s, varl, t, e)  ->
            let fn =
              try newFunction fi (id_make s) true
              with Exit _ -> raise Terminate
            in
            forwardFunction fn;
            openScope(); (* new scope for the args of forward functions *)
            List.iter (fun x -> P.ignore (new_parameter fn x)) varl;
            closeScope();
            endFunctionHeader fn (get t);
        | _ -> () (* for non-functions do nothing *)
      in
      List.iter (fun x -> P.ignore (find_forwards x)) vl; (* first traverse *)
      List.iter (fun x -> typeOfVardef true x) vl         (* second traverse *)


and typeOfTypedef = function
    TD_Type (fi, tl)   -> () (* TODO: Not supported yet *)
  | TD_TDefId _  -> () (* TODO *)
  | TD_Constr _  -> () (* TODO *)

and typeOfVardef rec_flag = function
    VAR_Id (fi, s, varl, t, e) ->
      begin
        match varl with
        | [] -> (* var list empty, so we found a variable definition *)
            (* add a new_variable Entry to the current scope *)
            P.ignore (newVariable (id_make s) (get t) true);
            (* now we hide the definitions while we're processing the body *)
            if not rec_flag then hideScope (!currentScope) true;
            P.ignore (typeOfExpr e);
            hideScope (!currentScope) false
        | _ -> (* var list not empty, so we found a function definition *)
            (* we add a new_function Entry to the current scope *)
            let fn =
              try newFunction fi (id_make s) true
              with Exit _ -> raise Terminate
            in
            (* in case of let we hide the definition from the body *)
            if not rec_flag then hideScope (!currentScope) true;
            openScope(); (* new scope for the args and body definition *)
            (* now we add the parameters of the function *)
            List.iter (fun x -> P.ignore (new_parameter fn x)) varl;
            endFunctionHeader fn (get t); (* end of function header *)
            P.ignore (typeOfExpr e);
            closeScope(); (* close the body's scope *)
            hideScope (!currentScope) false;
      end
  | VAR_MutId (fi, s, t, exprl) ->
      begin
        match exprl with
        | None -> (* simple variable definition *)
            (* add a new_variable Entry to the current scope *)
            P.ignore (newVariable (id_make s) (TY_Ref (get t)) true)
        | Some es -> (* array variable definition *)
            (* es types must be integers *)
            let check_array_dim ty =
              if (=) ty TY_Int then ()
              else error fi 3 "Array exprs should be integers.\n"
            in List.iter (fun x -> check_array_dim (typeOfExpr x)) es;
            P.ignore (newVariable (id_make s)
              (TY_Array (List.length es, get t)) true)
      end

and typeOfExpr = function
    E_Unit _      -> TY_Unit
  | E_True _      -> TY_Bool
  | E_False _     -> TY_Bool
  | E_LitInt _    -> TY_Int
  | E_LitChar _   -> TY_Char
  | E_LitFloat _  -> TY_Float
  | E_LitId (fi, id)    ->
      let l = lookupEntry fi (id_make id) LOOKUP_ALL_SCOPES true in
      begin
        match l.entry_info with
        | ENTRY_variable v -> v.variable_type;
        | ENTRY_parameter p -> p.parameter_type;
        | ENTRY_function f -> f.function_result;
        | _ -> error fi 3 "E_LitId not found"
      end
  | E_LitConstr (fi, id) -> (* XXX: check if it's right *)
      let l = lookupEntry fi (id_make id) LOOKUP_ALL_SCOPES true in
      begin
        match l.entry_info with
        | ENTRY_variable v -> v.variable_type;
        | ENTRY_parameter p -> p.parameter_type;
        | _ -> error fi 3 "E_LitId not found"
      end
  | E_LitString (fi, s) -> TY_Array (1, TY_Char) (*FIXME: change 1 to String.length s*)
  | E_UPlus (fi, e)     ->
      if (=) (typeOfExpr e) TY_Int then TY_Int
      else error fi 3 "Type mismatch"
  | E_UFPlus (fi, e)    ->
      if (=) (typeOfExpr e) TY_Float then TY_Float
      else error fi 3 "Type mismatch"
  | E_UMinus (fi, e)    ->
      if (=) (typeOfExpr e) TY_Int then TY_Int
      else error fi 3 "Type mismatch"
  | E_UFMinus (fi, e)   ->
      if (=) (typeOfExpr e) TY_Float then TY_Float
      else error fi 3 "Type mismatch"
  | E_Not (fi, e)       ->
      if (=) (typeOfExpr e) TY_Bool then TY_Bool
      else error fi 3 "Type mismatch"
  | E_Deref (fi, e)     -> typeOfExpr e
  | E_New (fi, t)       ->
      if isNotArrayOrFunc t then t
      else error fi 3 "Type mismatch"
  | E_Delete (fi, e)    -> (* FIXME: check that mem was allocated dynamically *)
      if isRef (typeOfExpr e) then TY_Unit
      else error fi 3 "Type mismatch"
  | E_Block (fi, e)     -> typeOfExpr e
  | E_Plus (fi, e1, e2) ->
      if (=) (typeOfExpr e1) TY_Int then
        let typ2 = typeOfExpr e2 in
        if (=) typ2 TY_Int then TY_Int
        else error fi 3 "Type mismatch"
      else error fi 3 "Type mismatch"
  | E_FPlus (fi, e1, e2)  ->
      if (=) (typeOfExpr e1) TY_Float then
        let typ2 = typeOfExpr e2 in
        if (=) typ2 TY_Float then TY_Float
        else error fi 3 "Type mismatch"
      else error fi 3 "Type mismatch"
  | E_Minus (fi, e1, e2)  ->
      if (=) (typeOfExpr e1) TY_Int then
        let typ2 = typeOfExpr e2 in
        if (=) typ2 TY_Int then TY_Int
        else error fi 3 "Type mismatch"
      else error fi 3 "Type mismatch"
  | E_FMinus (fi, e1, e2) ->
      if (=) (typeOfExpr e1) TY_Float then
        let typ2 = typeOfExpr e2 in
        if (=) typ2 TY_Float then TY_Float
        else error fi 3 "Type mismatch"
      else error fi 3 "Type mismatch"
  | E_Mul (fi, e1, e2)    ->
      if (=) (typeOfExpr e1) TY_Int then
        let typ2 = typeOfExpr e2 in
        if (=) typ2 TY_Int then TY_Int
        else error fi 3 "Type mismatch"
      else error fi 3 "Type mismatch"
  | E_FMul (fi, e1, e2)   ->
      if (=) (typeOfExpr e1) TY_Float then
        let typ2 = typeOfExpr e2 in
        if (=) typ2 TY_Float then TY_Float
        else error fi 3 "Type mismatch"
      else error fi 3 "Type mismatch"
  | E_Div (fi, e1, e2)     ->
      if (=) (typeOfExpr e1) TY_Int then
        let typ2 = typeOfExpr e2 in
        if (=) typ2 TY_Int then TY_Int
        else error fi 3 "Type mismatch"
      else error fi 3 "Type mismatch"
  | E_FDiv (fi, e1, e2)     ->
      if (=) (typeOfExpr e1) TY_Float then
        let typ2 = typeOfExpr e2 in
        if (=) typ2 TY_Float then TY_Float
        else error fi 3 "Type mismatch"
      else error fi 3 "Type mismatch"
  | E_Mod (fi, e1, e2)      ->
      if (=) (typeOfExpr e1) TY_Int then
        let typ2 = typeOfExpr e2 in
        if (=) typ2 TY_Int then TY_Int
        else error fi 3 "Type mismatch"
      else error fi 3 "Type mismatch"
  | E_Pow (fi, e1, e2)       ->
      if (=) (typeOfExpr e1) TY_Float then
        let typ2 = typeOfExpr e2 in
        if (=) typ2 TY_Float then TY_Float
        else error fi 3 "Type mismatch"
      else error fi 3 "Type mismatch"
  | E_Eq (fi, e1, e2)         ->
      let typ1 = typeOfExpr e1 in
      if isNotArrayOrFunc typ1 then
        if (=) typ1 (typeOfExpr e2) then TY_Bool
        else error fi 3 "Type mismatch (e1 = e2)"
      else error fi 3 "Type mismatch (e1 =) array-func type"
  | E_Differ (fi, e1, e2)     ->
      let typ1 = typeOfExpr e1 in
      if isNotArrayOrFunc typ1 then
        if (=) typ1 (typeOfExpr e2) then TY_Bool
        else error fi 3 "Type mismatch (e1 <> e2)"
      else error fi 3 "Type mismatch (e1 <>) array-func type"
  | E_Equal (fi, e1, e2)       ->
      let typ1 = typeOfExpr e1 in
      if isNotArrayOrFunc typ1 then
        if (=) typ1 (typeOfExpr e2) then TY_Bool
        else error fi 3 "Type mismatch (e1 == e2)"
      else error fi 3 "Type mismatch (e1 ==) array-func type"
  | E_NEqual (fi, e1, e2)      ->
      let typ1 = typeOfExpr e1 in
      if isNotArrayOrFunc typ1 then
        if (=) typ1 (typeOfExpr e2) then TY_Bool
        else error fi 3 "Type mismatch (e1 != e2)"
      else error fi 3 "Type mismatch (e1 !=) array-func type"
  | E_Lt (fi, e1, e2)          ->
      let typ1 = typeOfExpr e1 in
      if isSimpleType typ1 then
        if (=) typ1 (typeOfExpr e2) then TY_Bool
        else error fi 3 "Type mismatch"
      else error fi 3 "Type mismatch - not simple type"
  | E_Gt (fi, e1, e2)          ->
      let typ1 = typeOfExpr e1 in
      if isSimpleType typ1 then
        if (=) typ1 (typeOfExpr e2) then TY_Bool
        else error fi 3 "Type mismatch"
      else error fi 3 "Type mismatch - not simple type"
  | E_Leq (fi, e1, e2)         ->
      let typ1 = typeOfExpr e1 in
      if isSimpleType typ1 then
        if (=) typ1 (typeOfExpr e2) then TY_Bool
        else error fi 3 "Type mismatch"
      else error fi 3 "Type mismatch - not simple type"
  | E_Geq (fi, e1, e2)         ->
      let typ1 = typeOfExpr e1 in
      if isSimpleType typ1 then
        if (=) typ1 (typeOfExpr e2) then TY_Bool
        else error fi 3 "Type mismatch"
      else error fi 3 "Type mismatch - not simple type"
  | E_Andlogic (fi, e1, e2)    ->
      if (=) (typeOfExpr e1) TY_Bool then
        if (=) (typeOfExpr e2) TY_Bool then TY_Bool
        else error fi 3 "Type mismatch"
      else error fi 3 "Type mismatch - not bool"
  | E_Orlogic (fi, e1, e2)     ->
      if (=) (typeOfExpr e1) TY_Bool then
        if (=) (typeOfExpr e2) TY_Bool then TY_Bool
        else error fi 3 "Type mismatch"
      else error fi 3 "Type mismatch - not bool"
  | E_Assign (fi, e1, e2)      -> (* FIXME: check if e1 is l-value *)
      let typ = typeOfExpr e1 in
      if isRef typ then
        if (=) typ (TY_Ref (typeOfExpr e2)) then TY_Unit
        else error fi 3 "Type mismatch"
      else error fi 3 "Type mismatch - ty_ref"
  | E_Semicolon (fi, e1, e2)   -> typeOfExpr e2 (* XXX: ignoring the first one *)
  | E_While (fi, e1, e2)       ->
      if isBool (typeOfExpr e1) then
        if isUnit (typeOfExpr e2) then TY_Unit
        else error fi 3 "Type mismatch - not unit"
      else error fi 3 "Type mismatch - not bool"
  | E_For (fi, s, ti, e1, e2, e)  ->
      let typ1 = typeOfExpr e1 in
      let typ2 = typeOfExpr e2 in
      if (=) typ1 typ2 then
        if isUnit (typeOfExpr e) && typ1 = TY_Int then TY_Unit
        else error fi 3 "Type mismatch"
      else error fi 3 "Type mismatch t1 t2"
  | E_Match (fi, e, clauses)   -> TY_Unit (* TODO *)
  | E_IfStmt (fi, e, e1, e2)   ->
      if isBool (typeOfExpr e) then
        let typ1 = typeOfExpr e1 in
        begin
          match e2 with
          | Some _e2 ->
              if (=) typ1 (typeOfExpr _e2) then typ1
              else let fi2 = get_info_expr _e2 in
                   error fi2 3 "This expression has wrong type"
          | None -> typ1
        end
      else let fi1 = get_info_expr e in
           error fi1 3 "This expression has wrong type (bool expected)"
  | E_LetIn (fi, ld, e)        -> (* local declarations *)
      typeOfLetdef ld;
      let typ = typeOfExpr e in
      closeScope();
      typ
  | E_Dim (fi, i, s)           -> TY_Int (* TODO *)
  | E_Call (fi, s, el)         ->
      let en = lookupEntry fi (id_make s) LOOKUP_ALL_SCOPES true in
      begin
        match en.entry_info with
        | ENTRY_function info -> begin
            let typ = info.function_result in
            let pars = info.function_paramlist in
            let param_type p =
              match p.entry_info with
              | ENTRY_parameter inf -> inf.parameter_type
              | _ -> error fi 3 "call: params expected"
            in
            let check_call_args a b =
              if (=) (typeOfExpr a) b then ()
              else error fi 3 "different call-arg types."
            in
            try
              List.iter2 (fun x y -> check_call_args x (param_type y)) el pars;
              typ
            with Invalid_argument e ->
              error fi 3 "different number of args in call."
        end
        | _ -> error fi 3 "call name not a func"
      end
  | E_Constructor (fi, s, el)  -> TY_Unit (* TODO *)
  | E_ArrayEl (fi, s, el)      ->
      match el with
      | [] -> TY_Int
      | (e :: es) ->
          if (=) (typeOfExpr e) TY_Int then typeOfExpr (E_ArrayEl (fi, s, es))
          else error fi 3 "Type mismatch (array_el) not int"

(* and typeOfClause = function *)

(* and typeOfPattern = function
    P_True        -> TY_Bool
  | P_False       -> TY_Bool
  | P_LitId id    -> TY_Unit (* TODO *)
  | P_LitChar c   -> TY_Char
  | P_LitFloat f  -> TY_Char
  | P_Plus _      -> TY_Int
  | P_FPlus _     -> TY_Float
  | P_Minus _     -> TY_Int
  | P_FMinus _    -> TY_Float
  | P_LitConstr _ -> TY_Unit (* TODO: not supported yet *)
*)
;;

