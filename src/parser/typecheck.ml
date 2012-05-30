(* Type checking and semantic analysis *)

open Pervasives

module P = Pervasives

open Identifier
open Symbol
open Error
open Types
open Ast

let new_parameter f = function
    VAR_Id (s, [], t, e) ->
      begin
        match t with
        | None -> (error "No typeinfer you must provide a type\n";
                  raise Terminate)
        | Some _t ->
            newParameter (id_make s) _t PASS_BY_VALUE f true
      end
  | _ -> (error "Not a valid parameter form\n"; raise Terminate)
;;

exception Terminate

let rec typeOfExpr = function
    E_Unit        -> TY_Unit
  | E_True        -> TY_Bool
  | E_False       -> TY_Bool
  | E_LitInt _    -> TY_Int
  | E_LitChar _   -> TY_Char
  | E_LitFloat _  -> TY_Float
  | E_LitId id    ->
      let l = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
      begin
        match l.entry_info with
        | ENTRY_variable v -> v.variable_type;
        | ENTRY_parameter p -> p.parameter_type;
        | _ -> (error "E_LitId not found"; raise Terminate)
      end
  | E_LitConstr id -> (* XXX *)
      let l = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
      begin
        match l.entry_info with
        | ENTRY_variable v -> v.variable_type;
        | ENTRY_parameter p -> p.parameter_type;
        | _ -> (error "E_LitId not found"; raise Terminate)
      end
  | E_LitString s -> TY_Array (String.length s, TY_Char)
  | E_UPlus e     ->
      if (=) (typeOfExpr e) TY_Int then TY_Int
      else (error "Type mismatch (U +)"; raise Terminate)
  | E_UFPlus e    ->
      if (=) (typeOfExpr e) TY_Float then TY_Float
      else (error "Type mismatch (U +.)"; raise Terminate)
  | E_UMinus e    ->
      if (=) (typeOfExpr e) TY_Int then TY_Int
      else (error "Type mismatch (U -)"; raise Terminate)
  | E_UFMinus e   ->
      if (=) (typeOfExpr e) TY_Float then TY_Float
      else (error "Type mismatch (U -.)"; raise Terminate)
  | E_Not e       ->
      if (=) (typeOfExpr e) TY_Bool then TY_Bool
      else (error "Type mismatch (not)"; raise Terminate)
  | E_Deref e     -> typeOfExpr e
  | E_New t       ->
      if isNotArrayOrFunc t then t
      else (error "Type mismatch (new)"; raise Terminate)
  | E_Delete e    -> (* FIXME: check that mem was allocated dynamically *)
      if isRef (typeOfExpr e) then TY_Unit
      else (error "Type mismatch (delete)"; raise Terminate)
  | E_Block e     -> typeOfExpr e
  | E_Plus (e1, e2)        ->
      if (=) (typeOfExpr e1) TY_Int then
        let typ2 = typeOfExpr e2 in
        if (=) typ2 TY_Int then TY_Int
        else (error "Type mismatch (+ e2)"; raise Terminate)
      else (error "Type mismatch (e1 +)"; raise Terminate)
  | E_FPlus (e1, e2)       ->
      if (=) (typeOfExpr e1) TY_Float then
        let typ2 = typeOfExpr e2 in
        if (=) typ2 TY_Float then TY_Float
        else (error "Type mismatch (+. e2)"; raise Terminate)
      else (error "Type mismatch (e1 +.)"; raise Terminate)
  | E_Minus (e1, e2)       ->
      if (=) (typeOfExpr e1) TY_Int then
        let typ2 = typeOfExpr e2 in
        if (=) typ2 TY_Int then TY_Int
        else (error "Type mismatch (- e2)"; raise Terminate)
      else (error "Type mismatch (e1 -)"; raise Terminate)
  | E_FMinus (e1, e2)      ->
      if (=) (typeOfExpr e1) TY_Float then
        let typ2 = typeOfExpr e2 in
        if (=) typ2 TY_Float then TY_Float
        else (error "Type mismatch (-. e2)"; raise Terminate)
      else (error "Type mismatch (e1 -.)"; raise Terminate)
  | E_Mul (e1, e2)         ->
      if (=) (typeOfExpr e1) TY_Int then
        let typ2 = typeOfExpr e2 in
        if (=) typ2 TY_Int then TY_Int
        else (error "Type mismatch (* e2)"; raise Terminate)
      else (error "Type mismatch (e1 *)"; raise Terminate)
  | E_FMul (e1, e2)        ->
      if (=) (typeOfExpr e1) TY_Float then
        let typ2 = typeOfExpr e2 in
        if (=) typ2 TY_Float then TY_Float
        else (error "Type mismatch (*. e2)"; raise Terminate)
      else (error "Type mismatch (e1 *.)"; raise Terminate)
  | E_Div (e1, e2)         ->
      if (=) (typeOfExpr e1) TY_Int then
        let typ2 = typeOfExpr e2 in
        if (=) typ2 TY_Int then TY_Int
        else (error "Type mismatch (/ e2)"; raise Terminate)
      else (error "Type mismatch (e1 /)"; raise Terminate)
  | E_FDiv (e1, e2)        ->
      if (=) (typeOfExpr e1) TY_Float then
        let typ2 = typeOfExpr e2 in
        if (=) typ2 TY_Float then TY_Float
        else (error "Type mismatch (/. e2)"; raise Terminate)
      else (error "Type mismatch (e1 /.)"; raise Terminate)
  | E_Mod (e1, e2)         ->
      if (=) (typeOfExpr e1) TY_Int then
        let typ2 = typeOfExpr e2 in
        if (=) typ2 TY_Int then TY_Int
        else (error "Type mismatch (mod e2)"; raise Terminate)
      else (error "Type mismatch (e1 mod)"; raise Terminate)
  | E_Pow (e1, e2)         ->
      if (=) (typeOfExpr e1) TY_Float then
        let typ2 = typeOfExpr e2 in
        if (=) typ2 TY_Float then TY_Float
        else (error "Type mismatch (** e2)"; raise Terminate)
      else (error "Type mismatch (e1 **)"; raise Terminate)
  | E_Eq (e1, e2)          ->
      let typ1 = typeOfExpr e1 in
      if isNotArrayOrFunc typ1 then
        if (=) typ1 (typeOfExpr e2) then TY_Bool
        else (error "Type mismatch (e1 = e2)"; raise Terminate)
      else (error "Type mismatch (e1 =) array-func type"; raise Terminate)
  | E_Differ (e1, e2)      ->
      let typ1 = typeOfExpr e1 in
      if isNotArrayOrFunc typ1 then
        if (=) typ1 (typeOfExpr e2) then TY_Bool
        else (error "Type mismatch (e1 <> e2)"; raise Terminate)
      else (error "Type mismatch (e1 <>) array-func type"; raise Terminate)
  | E_Equal (e1, e2)       ->
      let typ1 = typeOfExpr e1 in
      if isNotArrayOrFunc typ1 then
        if (=) typ1 (typeOfExpr e2) then TY_Bool
        else (error "Type mismatch (e1 == e2)"; raise Terminate)
      else (error "Type mismatch (e1 ==) array-func type"; raise Terminate)
  | E_NEqual (e1, e2)      ->
      let typ1 = typeOfExpr e1 in
      if isNotArrayOrFunc typ1 then
        if (=) typ1 (typeOfExpr e2) then TY_Bool
        else (error "Type mismatch (e1 != e2)"; raise Terminate)
      else (error "Type mismatch (e1 !=) array-func type"; raise Terminate)
  | E_Lt (e1, e2)          ->
      let typ1 = typeOfExpr e1 in
      if isSimpleType typ1 then
        if (=) typ1 (typeOfExpr e2) then TY_Bool
        else (error "Type mismatch (e1 < e2)"; raise Terminate)
      else (error "Type mismatch (e1 <) not simple type"; raise Terminate)
  | E_Gt (e1, e2)          ->
      let typ1 = typeOfExpr e1 in
      if isSimpleType typ1 then
        if (=) typ1 (typeOfExpr e2) then TY_Bool
        else (error "Type mismatch (e1 > e2)"; raise Terminate)
      else (error "Type mismatch (e1 >) not simple type"; raise Terminate)
  | E_Leq (e1, e2)         ->
      let typ1 = typeOfExpr e1 in
      if isSimpleType typ1 then
        if (=) typ1 (typeOfExpr e2) then TY_Bool
        else (error "Type mismatch (e1 <= e2)"; raise Terminate)
      else (error "Type mismatch (e1 <=) not simple type"; raise Terminate)
  | E_Geq (e1, e2)         ->
      let typ1 = typeOfExpr e1 in
      if isSimpleType typ1 then
        if (=) typ1 (typeOfExpr e2) then TY_Bool
        else (error "Type mismatch (e1 >= e2)"; raise Terminate)
      else (error "Type mismatch (e1 >=) not simple type"; raise Terminate)
  | E_Andlogic (e1, e2)    ->
      if (=) (typeOfExpr e1) TY_Bool then
        if (=) (typeOfExpr e2) TY_Bool then TY_Bool
        else (error "Type mismatch (e1 && e2)"; raise Terminate)
      else (error "Type mismatch (e1 &&) not bool"; raise Terminate)
  | E_Orlogic (e1, e2)     ->
      if (=) (typeOfExpr e1) TY_Bool then
        if (=) (typeOfExpr e2) TY_Bool then TY_Bool
        else (error "Type mismatch (e1 ||  e2)"; raise Terminate)
      else (error "Type mismatch (e1 ||) not bool"; raise Terminate)
  | E_Assign (e1, e2)      -> (* FIXME: check if e1 is l-value *)
      let typ = typeOfExpr e1 in
      if isRef typ then
        if (=) typ (TY_Ref (typeOfExpr e2)) then TY_Unit
        else (error "Type mismatch (:=)"; raise Terminate)
      else (error "Type mismatch (:=) ty_ref"; raise Terminate)
  | E_Semicolon (e1, e2)   -> typeOfExpr e2 (* XXX:ignoring the first one *)
  | E_While (e1, e2)       ->
      if isBool (typeOfExpr e1) then
        if isUnit (typeOfExpr e2) then TY_Unit
        else (error "Type mismatch (while) not unit"; raise Terminate)
      else (error "Type mismatch (while) not bool"; raise Terminate)
  | E_For (s, ti, e1, e2, e)  ->
      let typ1 = typeOfExpr e1 in
      let typ2 = typeOfExpr e2 in
      if (=) typ1 typ2 then
        if isUnit (typeOfExpr e) && typ1 = TY_Int then TY_Unit
        else (error "Type mismatch (for)"; raise Terminate)
      else (error "Type mismatch (for) t1 t2"; raise Terminate)
  | E_Match (e1, e2)       -> TY_Unit (* TODO *)
  | E_IfStmt (e, e1, e2)   ->
      if isBool (typeOfExpr e) then
        let typ1 = typeOfExpr e1 in
        begin
          match e2 with
          | Some _e2 ->
              if (=) typ1 (typeOfExpr _e2) then typ1
              else (error "Type mismatch (if-else)"; raise Terminate)
          | None -> typ1
        end
      else (error "Type mismatch (if) not bool"; raise Terminate)
  | E_LetIn (ld, e)        -> typeOfExpr e (* FIXME: scope fix *)
  | E_Dim (i, s)           -> TY_Int
  | E_Call (s, el)         -> TY_Unit (* TODO *)
  | E_Constructor (s, el)  -> TY_Unit (* TODO *)
  | E_ArrayEl (s, el)      ->
      match el with
      | [] -> TY_Int
      | (e :: es) ->
          if (=) (typeOfExpr e) TY_Int then typeOfExpr (E_ArrayEl (s, es))
          else (error "Type mismatch (array_el) not int"; raise Terminate)
;;

let rec typeOf = function
    PROGRAM (ldfs, tdfs) ->
      List.iter (fun x -> typeOfLetdef x) ldfs;
      List.iter (fun x -> typeOfTypedef x) tdfs

and typeOfLetdef = function
  (* let: open a new scope and process it's definitions. 
   * rec_flag := false*)
    L_Let vl    ->
      openScope();
      List.iter (fun x -> typeOfVardef false x) vl
  (* let rec: open a new scope, then find the forward declaration 
   * of the list's head, (currently the name of function), and then
   * process it's definitions. rec_flag := true *)
  | L_LetRec vl ->
      openScope();
(*      P.ignore (forwardFunction (List.hd vl)); (* FIXME *) *)
      List.iter (fun x -> typeOfVardef false x) vl

and typeOfTypedef = function
    TD_Type tl   -> () (* TODO *)
  | TD_TDefId _  -> () (* TODO *)
  | TD_Constr _  -> () (* TODO *)

and typeOfVardef rec_flag = function
    VAR_Id (s, vl, t, e) ->
      (match vl with
      (* arg list empyt, so we found a variable *)
      | [] ->
          begin
            (* add a new variable to the current scope *)
            P.ignore(newVariable (id_make s) (get t) true);
            (* because definition of the form 'let rec x = e' is not
             * allowed (that means we came from let), we should hide 
             * the current scope while we are processing the expr. *)
            hideScope (!currentScope) true;
            P.ignore(typeOfExpr e);
            hideScope (!currentScope) false
          end
      (* arg list not empty, so we found function arguments *)
      | _ ->
          begin
            (* we make a new function entry to the current scope *)
            let f =
              try newFunction (id_make s) true
              with Exit -> raise Terminate
            in
            (* we add all function params to the above's function scope *)
            List.iter (fun x -> P.ignore (new_parameter f x)) vl;
            endFunctionHeader f (get t); (* FIXME: check type t *)
            (* in case of a let rec we shouldn't hide the body while we
             * are processing the expr.  *)
            if rec_flag then ()
            else (hideScope (!currentScope) true;
                  P.ignore(typeOfExpr e);
                  hideScope (!currentScope) false)
          end)
  | VAR_MutId (s, t, el) -> () (* TODO *)
  | VAR_Formal (s, t)    -> () (* TODO *)
;;
