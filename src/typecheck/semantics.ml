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

(* Support functions *)

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
    VAR_Id (sem, fi, s, [], t, e) ->
      begin
        match t with
        | None -> error fi 3 "No type inferer support, you must provide a type"
        | Some _t -> newParameter fi (id_make s) _t PASS_BY_VALUE f true
      end
  | _ -> (* FIXME: currying (function parameters).
          * eg. VAR_Id (sem, ,s, lst, t, e) *)
      err "Invalid parameter form"
;;

(* Initialize Symbol Table and other initializations *)
let () =
  initSymbolTable 512;
  (* first add standard library to the Main's scope *)
  List.iter function_create library_functions

let rec typeOf = function
    PROGRAM (ldfs, tdfs) ->
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
    L_Let (sem, fi, vl)    -> (* vl: vardefs connected with 'and' keyword *)
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
  | L_LetRec (sem, fi, vl) -> (* vl: vardefs connected with 'and' keyword *)
      openScope(); (* scope for the definition only *)
      let find_forwards = function
          VAR_Id (sem, info, s, varl, t, e)  ->
            let fn =
              try newFunction info (id_make s) true
              with Exit _ -> raise Terminate
            in
            forwardFunction fn;
            openScope(); (* new scope for the args of forward functions *)
            List.iter (fun x -> P.ignore (new_parameter fn x)) varl;
            closeScope();
            endFunctionHeader fn (get t);
        | _ -> () (* we do nothing for non-functions *)
      in
      List.iter (fun x -> P.ignore (find_forwards x)) vl; (* first traverse *)
      List.iter (fun x -> typeOfVardef true x) vl         (* second traverse *)


and typeOfTypedef = function
    TD_Type (sem, fi, tl)       -> (* TODO: Not supported yet *)
      error fi 3 "user defined data types are not supported"
  | TD_TDefId (sem, fi, s, tl)  -> (* TODO: Not supported yet *)
      error fi 3 "user defined data types are not supported"
  | TD_Constr (sem, fi, s, tyl) -> (* TODO: Not supported yet *)
      error fi 3 "user defined data types are not supported"

and typeOfVardef rec_flag = function
    VAR_Id (sem, fi, s, varl, t, e) ->
      begin
        match varl with
        | [] -> (* var list empty, so we found a variable definition *)
            (* add a new_variable Entry to the current scope *)
            P.ignore (newVariable fi (id_make s) (get t) true);
            (* now we hide the definitions while we're processing the body *)
            if not rec_flag then hideScope (!currentScope) true;
            (* function body must conforms with the type declared *)
            if not (equalType (get t) (typeOfExpr e)) then (
              let str1 = "type mismatch in definition\n" in
              let str = "the type of constant does not match the declared \
                        type:" in
              error_args fi (str1 ^ str) (id_make s);
              raise (Exit 3));
            (* now we unhide the scope if we've hidden it before *)
            if not rec_flag then hideScope (!currentScope) false
        | _ -> (* var list not empty, so we found a function definition *)
            match (get t) with
            | TY_Function _ ->
                let str1 = "type mismatch in definition\n" in
                let str = "function returns a function:" in
                error_args fi (str1 ^ str) (id_make s);
                raise (Exit 3)
            | _ -> (* add a new_function Entry to the current scope *)
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
              (* function body must conforms with the type declared *)
              let typ = typeOfExpr e in
              if not (equalType (get t) typ) then (
                let str1 = "type mismatch in definition\n" in
                let str = "the body of function does not match the declared \
                          type:" in
                error_args fi (str1 ^ str) (id_make s);
                raise (Exit 3));
              closeScope(); (* close the body's scope *)
              (* now we unhide the scope if we've hidden it before *)
              if not rec_flag then hideScope (!currentScope) false
      end
  | VAR_MutId (sem, fi, s, t, exprl) ->
      begin
        match (get t) with
        | TY_Array _ ->
            let str = "References to arrays are not allowed: " in
            error_args fi str (id_make s)
        | _ as type_ ->
            match exprl with
              None -> (* simple variable definition *)
                (* add a new_variable Entry to the current scope *)
                P.ignore (newVariable fi (id_make s) (TY_Ref type_) true)
            | Some es -> (* array variable definition *)
                (* es types must be integers *)
                let check_array_dim ty =
                  if (=) ty TY_Int then ()
                  else error fi 3 "Array exprs should be integers.\n"
                in
                List.iter (fun x -> check_array_dim (typeOfExpr x)) es;
                P.ignore (newVariable fi (id_make s)
                (TY_Array (List.length es, type_)) true)
      end

and typeOfExpr = function
  (* Constants Operators *)
    E_Unit (sem, info)        -> TY_Unit
  | E_True (sem, info)        -> TY_Bool
  | E_False (sem, info)       -> TY_Bool
  | E_LitInt (sem, info, _)   -> TY_Int
  | E_LitChar (sem, info, _)  -> TY_Char
  | E_LitFloat (sem, info, _) -> TY_Float
  | E_LitString (sem, fi, s) -> TY_Array (1, TY_Char)
  (* Names (constants, functions, parameters, constructors, expressions) *)
  | E_LitId (sem, fi, id)    ->
      let l = lookupEntry fi (id_make id) LOOKUP_ALL_SCOPES true in
      begin
        match l.entry_info with
        | ENTRY_variable v -> v.variable_type;
        | ENTRY_parameter p -> p.parameter_type;
        | ENTRY_function f ->
            let params = List.map (
              fun en ->
                match en.entry_info with
                | ENTRY_parameter par_info -> par_info.parameter_type;
                | _ -> error fi 3 "Wrong function entry"
              ) f.function_paramlist
            in TY_Function(params, f.function_result);
        | _ ->
            let str = "E_LitId not found (is not func, param or var):" in
            error_args fi str (id_make id);
            raise (Exit 3)
      end
  | E_LitConstr (sem, fi, id) -> (* TODO: not supported yet *)
      (*
      let l = lookupEntry fi (id_make id) LOOKUP_ALL_SCOPES true in
      begin
        match l.entry_info with
        | ENTRY_variable v -> v.variable_type;
        | ENTRY_parameter p -> p.parameter_type;
        | _ -> error fi 3 "E_LitConstr not found"
      end
      *)
      error fi 3 "user defined date types are not supported"
  (* Unary Arithmetic Operators *)
  | E_UPlus (sem, fi, e)     ->
      if (=) (typeOfExpr e) TY_Int then TY_Int
      else error fi 3 "Type mismatch, TY_Int expected"
  | E_UFPlus (sem, fi, e)    ->
      if (=) (typeOfExpr e) TY_Float then TY_Float
      else error fi 3 "Type mismatch, TY_Float"
  | E_UMinus (sem, fi, e)    ->
      if (=) (typeOfExpr e) TY_Int then TY_Int
      else error fi 3 "Type mismatch, TY_Int expected"
  | E_UFMinus (sem, fi, e)   ->
      if (=) (typeOfExpr e) TY_Float then TY_Float
      else error fi 3 "Type mismatch, TY_Float expected"
  (* References and assigments *)
  | E_Assign (sem, fi, e1, e2)      -> (* FIXME: check if e1 is l-value *)
      let typ = typeOfExpr e1 in
      if isRef typ then
        if (=) typ (TY_Ref (typeOfExpr e2)) then TY_Unit
        else error fi 3 "Type mismatch, right operand"
      else error fi 3 "Type mismatch, left operand"
  | E_Deref (sem, fi, e)     ->
     begin
      match typeOfExpr e with
      | TY_Ref ty_ -> ty_
      | _          -> error fi 3 "deref should be ref type"
     end
  (* Memory Dynamic Allocation *)
  | E_New (sem, fi, t)       ->
      if isNotArrayOrFunc t then TY_Ref t
      else error fi 3 "Type mismatch, cannot allocate TY_Array or TY_Function."
  | E_Delete (sem, fi, e)    -> (* FIXME: check that mem was allocated dynamically *)
      if isRef (typeOfExpr e) then TY_Unit
      else error fi 3 "Type mismatch, only TY_Ref can be unallocated."
  (* Binary Integer Arithmetic Operators *)
  | E_Plus (sem, fi, e1, e2) ->
      if (=) (typeOfExpr e1) TY_Int then
        let typ2 = typeOfExpr e2 in
        if (=) typ2 TY_Int then TY_Int
        else error fi 3 "Type mismatch, TY_Int expected"
      else error fi 3 "Type mismatch, TY_Int expected"
  | E_Minus (sem, fi, e1, e2)  ->
      if (=) (typeOfExpr e1) TY_Int then
        let typ2 = typeOfExpr e2 in
        if (=) typ2 TY_Int then TY_Int
        else error fi 3 "Type mismatch, TY_Int expected"
      else error fi 3 "Type mismatch, TY_Int expected"
  | E_Mul (sem, fi, e1, e2)    ->
      if (=) (typeOfExpr e1) TY_Int then
        let typ2 = typeOfExpr e2 in
        if (=) typ2 TY_Int then TY_Int
        else error fi 3 "Type mismatch, TY_Int expected"
      else error fi 3 "Type mismatch, TY_Int expected"
  | E_Div (sem, fi, e1, e2)     ->
      if (=) (typeOfExpr e1) TY_Int then
        let typ2 = typeOfExpr e2 in
        if (=) typ2 TY_Int then TY_Int
        else error fi 3 "Type mismatch, TY_Int expected"
      else error fi 3 "Type mismatch, TY_Int expected"
  | E_Mod (sem, fi, e1, e2)      ->
      if (=) (typeOfExpr e1) TY_Int then
        let typ2 = typeOfExpr e2 in
        if (=) typ2 TY_Int then TY_Int
        else error fi 3 "Type mismatch, TY_Int expected"
      else error fi 3 "Type mismatch, TY_Int expected"
  (* Binary Float Arithmetic Operators *)
  | E_FPlus (sem, fi, e1, e2)  ->
      if (=) (typeOfExpr e1) TY_Float then
        let typ2 = typeOfExpr e2 in
        if (=) typ2 TY_Float then TY_Float
        else error fi 3 "Type mismatch, TY_Float expected"
      else error fi 3 "Type mismatch, TY_Float expected"
  | E_FMinus (sem, fi, e1, e2) ->
      if (=) (typeOfExpr e1) TY_Float then
        let typ2 = typeOfExpr e2 in
        if (=) typ2 TY_Float then TY_Float
        else error fi 3 "Type mismatch, TY_Float expected"
      else error fi 3 "Type mismatch, TY_Float expected"
  | E_FMul (sem, fi, e1, e2)   ->
      if (=) (typeOfExpr e1) TY_Float then
        let typ2 = typeOfExpr e2 in
        if (=) typ2 TY_Float then TY_Float
        else error fi 3 "Type mismatch, TY_Float expected"
      else error fi 3 "Type mismatch, TY_Float expected"
  | E_FDiv (sem, fi, e1, e2)     ->
      if (=) (typeOfExpr e1) TY_Float then
        let typ2 = typeOfExpr e2 in
        if (=) typ2 TY_Float then TY_Float
        else error fi 3 "Type mismatch, TY_Float expected"
      else error fi 3 "Type mismatch, TY_Float expected"
  | E_Pow (sem, fi, e1, e2)       ->
      if (=) (typeOfExpr e1) TY_Float then
        let typ2 = typeOfExpr e2 in
        if (=) typ2 TY_Float then TY_Float
        else error fi 3 "Type mismatch"
      else error fi 3 "Type mismatch"
  (* Structural and Natural Equality Operators *)
  | E_Eq (sem, fi, e1, e2)         ->
      let typ1 = typeOfExpr e1 in
      if isNotArrayOrFunc typ1 then
        if (=) typ1 (typeOfExpr e2) then TY_Bool
        else error fi 3 "Type mismatch (e1 = e2)"
      else error fi 3 "Type mismatch (e1 =) array-func type"
  | E_Differ (sem, fi, e1, e2)     ->
      let typ1 = typeOfExpr e1 in
      if isNotArrayOrFunc typ1 then
        if (=) typ1 (typeOfExpr e2) then TY_Bool
        else error fi 3 "Type mismatch (e1 <> e2)"
      else error fi 3 "Type mismatch (e1 <>) array-func type"
  | E_Equal (sem, fi, e1, e2)       ->
      let typ1 = typeOfExpr e1 in
      if isNotArrayOrFunc typ1 then
        if (=) typ1 (typeOfExpr e2) then TY_Bool
        else error fi 3 "Type mismatch (e1 == e2)"
      else error fi 3 "Type mismatch (e1 ==) array-func type"
  | E_NEqual (sem, fi, e1, e2)      ->
      let typ1 = typeOfExpr e1 in
      if isNotArrayOrFunc typ1 then
        if (=) typ1 (typeOfExpr e2) then TY_Bool
        else error fi 3 "Type mismatch (e1 != e2)"
      else error fi 3 "Type mismatch (e1 !=) array-func type"
  | E_Lt (sem, fi, e1, e2)          ->
      let typ1 = typeOfExpr e1 in
      if isSimpleType typ1 then
        if (=) typ1 (typeOfExpr e2) then TY_Bool
        else error fi 3 "Type mismatch"
      else error fi 3 "Type mismatch - not simple type"
  | E_Gt (sem, fi, e1, e2)          ->
      let typ1 = typeOfExpr e1 in
      if isSimpleType typ1 then
        if (=) typ1 (typeOfExpr e2) then TY_Bool
        else error fi 3 "Type mismatch"
      else error fi 3 "Type mismatch - not simple type"
  | E_Leq (sem, fi, e1, e2)         ->
      let typ1 = typeOfExpr e1 in
      if isSimpleType typ1 then
        if (=) typ1 (typeOfExpr e2) then TY_Bool
        else error fi 3 "Type mismatch"
      else error fi 3 "Type mismatch - not simple type"
  | E_Geq (sem, fi, e1, e2)         ->
      let typ1 = typeOfExpr e1 in
      if isSimpleType typ1 then
        if (=) typ1 (typeOfExpr e2) then TY_Bool
        else error fi 3 "Type mismatch"
      else error fi 3 "Type mismatch - not simple type"
  (* Logical Operators *)
  | E_Not (sem, fi, e)       ->
      if (=) (typeOfExpr e) TY_Bool then TY_Bool
      else error fi 3 "Type mismatch, TY_Bool expected"
  | E_Andlogic (sem, fi, e1, e2)    ->
      if (=) (typeOfExpr e1) TY_Bool then
        if (=) (typeOfExpr e2) TY_Bool then TY_Bool
        else error fi 3 "Type mismatch, TY_Bool expected"
      else error fi 3 "Type mismatch, TY_Bool expected"
  | E_Orlogic (sem, fi, e1, e2)     ->
      if (=) (typeOfExpr e1) TY_Bool then
        if (=) (typeOfExpr e2) TY_Bool then TY_Bool
        else error fi 3 "Type mismatch, TY_Bool expected"
      else error fi 3 "Type mismatch, TY_Bool expected"
  (* Imperative Commands *)
  | E_Block (sem, fi, e)     -> typeOfExpr e
  | E_Semicolon (sem, fi, e1, e2)   -> P.ignore(typeOfExpr e1); typeOfExpr e2
  | E_While (sem, fi, e1, e2)       ->
      if isBool (typeOfExpr e1) then
        if isUnit (typeOfExpr e2) then TY_Unit
        else error fi 3 "Type mismatch, TY_Unit expected"
      else error fi 3 "Type mismatch, TY_Bool expected"
  | E_For (sem, fi, s, ti, e1, e2, e)  ->
      let typ1 = typeOfExpr e1 in
      let typ2 = typeOfExpr e2 in
      if (=) typ1 typ2 then
        begin
          openScope();
          P.ignore (newVariable fi (id_make s) TY_Int true);
          let e3 = typeOfExpr e in
          closeScope();
          if isUnit e3 && typ1 = TY_Int then TY_Unit
          else error fi 3 "Type mismatch"
        end
      else error fi 3 "Type mismatch, TY_Int expected"
  (* Decomposition of User Defined Types *)
  | E_Match (sem, fi, e, clauses)   ->
      (*
      let ex_type = typeOfExpr e in
      let res_type = ref TY_Unit  in
      let check_clauses typ_ clause =
        match clause with
        | P_Clause (info, pat, ex) ->
            openScope();
            if equalType (typeOfPattern pat) ex_type then
              (res_type := typeOfExpr ex; closeScope())
            else error info 3 "Wrong pattern type";
        | _ -> error fi 3 "Not clause form"
      in
      List.iter (fun cl -> check_clauses ex_type cl) clauses;
      !res_type
      *)
      error fi 3 "user defined data types are not supported"
  (* Local definitions *)
  | E_LetIn (sem, fi, ld, e)        -> (* local declarations *)
      typeOfLetdef ld;
      let typ = typeOfExpr e in
      closeScope(); (* close the clope that opened in letdef *)
      typ
  (* If statement *)
  | E_IfStmt (sem, fi, e, e1, e2)   ->
      if isBool (typeOfExpr e) then
        let typ1 = typeOfExpr e1 in
        begin
          match e2 with
          | Some _e2 ->
              if (=) typ1 (typeOfExpr _e2) then typ1
              else
                let fi2 = get_info_expr _e2 in
                error fi2 3 "This expression has wrong type"
          | None -> typ1 (* FIXME: should be TY_Unit (check p.12) *)
        end
      else
        let fi1 = get_info_expr e in
        error fi1 3 "This expression has wrong type (bool expected)"
  (* Array Elements and Dimensions *)
  | E_Dim (sem, fi, i, s)           ->
      let dm = get i in
      if (<=) dm 0 then error fi 3 "Negative array dimension"
      else
        begin
          let l = lookupEntry fi (id_make s) LOOKUP_ALL_SCOPES true in
            match l.entry_info with
            | ENTRY_variable v ->
                begin
                  match v.variable_type with
                  | TY_Array (len, type_) when dm <= len -> TY_Int
                  | TY_Array (len, type_) when dm > len ->
                      error fi 3 "Int exceeds array dimension"
                  | _ ->
                      let str = "Not an array type:" in
                      error_args fi str (id_make s);
                      raise (Exit 3)
                end
            | ENTRY_parameter p ->
                begin
                  match p.parameter_type with
                  | TY_Array (len, type_) when dm <= len -> TY_Int
                  | TY_Array (len, type_) when dm > len ->
                      error fi 3 "Int exceeds array dimension"
                  | _ ->
                      let str = "Not an array type:" in
                      error_args fi str (id_make s);
                      raise (Exit 3)
                end
            | _ ->
                let str = "Name is not a variable or parameter:" in
                error_args fi str (id_make s);
                raise (Exit 3)
        end
  | E_ArrayEl (sem, fi, s, el, el_len)      ->
      begin
        match el with
        | [] -> (* all expressions are integers, so look at the symbol table *)
            begin
              let en = lookupEntry fi (id_make s) LOOKUP_ALL_SCOPES true in
                match en.entry_info with
                | ENTRY_variable v ->
                    begin
                      match v.variable_type with
                      | TY_Array (len, type_) when len = el_len ->
                          TY_Ref type_
                      | TY_Array (len, type_) when len != el_len ->
                          error fi 3 "Array has wrong number of dimensions"
                      | _ ->
                          let str = "Not an array:" in
                          error_args fi str (id_make s);
                          raise (Exit 3)
                    end
                | ENTRY_parameter p ->
                    begin
                      match p.parameter_type with
                      | TY_Array (len, type_) when len = el_len ->
                          TY_Ref type_
                      | TY_Array (len, type_) when len != el_len ->
                          error fi 3 "Array has wrong number of dimensions"
                      | _ ->
                          let str = "Not an array:" in
                          error_args fi str (id_make s);
                          raise (Exit 3)
                    end
                | _ ->
                    let str = "Name is not a variable or parameter:" in
                    error_args fi str (id_make s);
                    raise (Exit 3)
                end
        | (e :: es) -> (* recursivly check array's expr if are TY_Int *)
          if (=) (typeOfExpr e) TY_Int
          then typeOfExpr (E_ArrayEl (sem, fi, s, es, el_len))
          else error fi 3 "Array indexes should be integers"
      end
  (* Function and Constructor call *)
  | E_Call (sem, fi, s, el)         ->
      let en = lookupEntry fi (id_make s) LOOKUP_ALL_SCOPES true in
      begin
        match en.entry_info with
        | ENTRY_function info ->
          begin
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
        | ENTRY_parameter p ->
          begin
            match p.parameter_type with
            | TY_Function (par_type_list, type_) ->
                let check_params real typical =
                  if equalType (typeOfExpr real) typical then ()
                  else
                    let str = "Funtion params type mismatch:" in
                    error_args fi str (id_make s)
                in
                List.iter2 (fun x y -> check_params x y) el par_type_list;
                type_
            | _ -> error fi 3 "wrong func-param type"
          end
        | _ ->
            let str = "Function name doesn't exist:" in
            error_args fi str (id_make s);
            raise (Exit 3)
      end
  | E_ConstrCall (sem, fi, s, el)  -> (* TODO: not supported yet *)
      error fi 3 "user defined data types are not supported"


and typeOfPattern = function
    P_True (sem, info)             -> TY_Bool
  | P_False (sem, info)            -> TY_Bool
  | P_LitId (sem, fi,id)           ->
      P.ignore (newVariable fi (id_make id) TY_Unit true);
      TY_Unit
  | P_LitChar (sem, fi, c)         -> TY_Char
  | P_LitFloat (sem, fi, f)        -> TY_Char
  | P_Plus (sem, fi, _)            -> TY_Int
  | P_FPlus (sem, fi, _)           -> TY_Float
  | P_Minus (sem, fi, _)           -> TY_Int
  | P_FMinus  (sem, fi, _)         -> TY_Float
  | P_LitConstr (sem, fi, s, patl) -> (* TODO: not supported yet *)
      error fi 3 "user defined data types are not supported"
  | _                 -> err "Wrong pattern form"
;;

