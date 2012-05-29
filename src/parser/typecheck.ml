(* Type checking and semantic analysis *)

open Identifier
open Symbol
open Error
open Types
open Ast

exception Terminate
(*
let rec typeOf =
 function
      PROGRAM (ld, td) ->
        typeOfLetdef ld; (* .. *)
        typeOfTypedef td (* .. *)

and typeOfLetdef =
  function
      L_Let vl    -> TY_Unit (* .. *)
    | L_LetRec vl -> TY_Unit (* .. *)

and typeOfTypedef =
  function
      TD_Type tl   -> TY_Unit (* .. *)
    | TD_TDefId tl -> TY_Unit (* .. *)
    | TD_Constr tl -> TY_Unit (* .. *)

and typeOfVardef =
  function
      VAR_Id (s, vl, t, e) -> TY_Unit (* .. *)
    | VAR_MutId (s, t, el) -> TY_Unit (* .. *)
    | VAR_Formal (s, t)    -> TY_Unit (* .. *)
*)
let rec typeOfExpr =
  function
      E_Unit        -> TY_Unit
    | E_True        -> TY_Bool
    | E_False       -> TY_Bool
    | E_LitInt _    -> TY_Int
    | E_LitChar _   -> TY_Char
    | E_LitFloat _  -> TY_Float
    | E_LitId id    -> (* XXX *)
        let l = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
        begin
          match l.entry_info with
          | ENTRY_variable v -> v.variable_type;
          | ENTRY_parameter p -> p.parameter_type;
          | _ -> exit 1
        end
    | E_LitConstr _ -> TY_Unit (* TODO *)
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
    | E_Delete e    -> (* TODO: check that mem was allocated dynamically *)
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
        else (error "Type mismatch (e1 =) array-function type"; raise Terminate)
    | E_Differ (e1, e2)      ->
        let typ1 = typeOfExpr e1 in
        if isNotArrayOrFunc typ1 then
          if (=) typ1 (typeOfExpr e2) then TY_Bool
          else (error "Type mismatch (e1 <> e2)"; raise Terminate)
        else (error "Type mismatch (e1 <>) array-function type"; raise Terminate)
    | E_Equal (e1, e2)       ->
        let typ1 = typeOfExpr e1 in
        if isNotArrayOrFunc typ1 then
          if (=) typ1 (typeOfExpr e2) then TY_Bool
          else (error "Type mismatch (e1 == e2)"; raise Terminate)
        else (error "Type mismatch (e1 ==) array-function type"; raise Terminate)
    | E_NEqual (e1, e2)      ->
        let typ1 = typeOfExpr e1 in
        if isNotArrayOrFunc typ1 then
          if (=) typ1 (typeOfExpr e2) then TY_Bool
          else (error "Type mismatch (e1 != e2)"; raise Terminate)
        else (error "Type mismatch (e1 !=) array-function type"; raise Terminate)
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
    | E_Assign (e1, e2)      -> (* TODO: check if e1 is l-value *)
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
    | E_LetIn (ld, e)        -> typeOfExpr e (* TODO: scope fix *)
    | E_Dim (i, s)           -> TY_Int
    | E_Call (s, el)         -> TY_Unit (* TODO *)
    | E_Constructor (s, el)  -> TY_Unit (* TODO *)
    | E_ArrayEl (s, el)      ->
        match el with
        | [] -> TY_Int
        | (e :: es) ->
            if (=) (typeOfExpr e) TY_Int then typeOfExpr (E_ArrayEl (s, es))
            else (error "Type mismatch (array_el) not int"; raise Terminate)

