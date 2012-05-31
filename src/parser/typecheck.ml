(* Type checking and semantic analysis *)

open Pervasives

module P = Pervasives

open Identifier
open Symbol
open Error
open Types
open Ast

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
  | E_Semicolon (e1, e2)   -> typeOfExpr e2 (* XXX: ignoring the first one *)
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

(* Every letfed block can re-define variables or functions that have been
 * defined in a previous letdef block.
 * So we wont't make a new scope everytime we found a new letdef but we'll
 * add it to the same scope (eg the_outer_scope). In case of duplicate values
 * we have to add the new entry to the symbol table and remove the old one. *)
and typeOfLetdef = function
  (* let:
   * In a let block the variable/function defined should NOT be visible by the
   * body of the block. So we have to make it hidden to the function body.
   * rec_flag := false *)
    L_Let vl    ->
      List.iter (fun x -> typeOfVardef false x) vl
  (* let rec:
   * In a let rec block the variable/function defined should BE visible by the
   * body of the block. So we have NOT make it hidden to the function body.
   * rec_flag := true *)
  | L_LetRec vl ->
        match vl with
        | (VAR_Id (s, varl, t, e) :: vls) ->
            let fn =
              try newFunction (id_make s) true
              with Exit -> raise Terminate
            in
            forwardFunction fn;
            List.iter (fun x -> typeOfVardef true x) vls
        | _ -> () (* XXX: no nothing for other types of var *)

and typeOfTypedef = function
    TD_Type tl   -> () (* TODO *)
  | TD_TDefId _  -> () (* TODO *)
  | TD_Constr _  -> () (* TODO *)

and typeOfVardef rec_flag = function
    VAR_Id (s, varl, t, e) ->
      begin
        match varl with
        | [] -> (* var list empty, so we found a variable definition *)
            (* add a new_variable Entry to the current scope *)
            P.ignore (newVariable (id_make s) (get t) true);
            (* because definition of the form 'let rec x = e' is not allowed
             * (that means we came from let), we should hide the current
             * scope while we are processing the expr (see comments above). *)
            hideScope (!currentScope) true;
            P.ignore (typeOfExpr e);
            hideScope (!currentScope) false
        | _ -> (* var list not empty, so we found a function definition *)
            (* we add a new_function Entry to the current scope *)
            let fn =
              try newFunction (id_make s) true
              with Exit -> raise Terminate
            in
            (* we add all function params to the above's function scope *)
            let new_parameter f = function
                VAR_Id (s, [], t, e) ->
                  begin
                    match t with
                    | None -> (error "No typeinfer you must provide a type\n";
                              raise Terminate)
                    | Some _t ->
                        newParameter (id_make s) _t PASS_BY_VALUE f true
                  end
              | _ -> (* FIXME: currying (function parameters).
                      * eg. VAR_Id (s, lst, t, e) *)
                  (error "Not a valid parameter form\n"; raise Terminate)
            in
            List.iter (fun x -> P.ignore (new_parameter fn x)) varl;
            endFunctionHeader fn (get t); (* FIXME: check type t *)
            (* in case of a let rec we shouldn't hide the scope while we are
             * processing the function body. *)
            if rec_flag then ()
            else (hideScope (!currentScope) true;
                  P.ignore (typeOfExpr e);
                  hideScope (!currentScope) false)
      end
  | VAR_MutId (s, t, exprl) ->
      begin
        match exprl with
        | None -> (* simple variable definition *)
            P.ignore (newVariable (id_make s) (TY_Ref (get t)) true)
        | Some es -> (* array variable definition *)
            (* es types must be integers *)
            let check_array_dim ty =
              if (=) ty TY_Int then ()
              else (error "Array exprs should be integers.\n"; raise Terminate)
            in List.iter (fun x -> check_array_dim (typeOfExpr x)) es;
            P.ignore (newVariable (id_make s)
              (TY_Array (List.length es, get t)) true)
      end
;;


