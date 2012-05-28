(* Type checking and semantic analysis *)

open Error
open Types
open Ast

exception Terminate

let rec typeOfExpr =
  function
    E_Unit        -> TY_Unit
  | E_True        -> TY_Bool
  | E_False       -> TY_Bool
  | E_LitInt _    -> TY_Int
  | E_LitChar _   -> TY_Char
  | E_LitFloat _  -> TY_Float
(*| E_LitId _     ->
  | E_LitConstr _ ->
  | E_LitString _ ->*)
  | E_New t       ->
      if isNotArrayFunc t then t
      else (error "Type mismatch (new)"; raise Terminate)
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
(*  | E_Delete e    -> *)
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
      if isNotArrayFunc typ1 then
        if (=) typ1 (typeOfExpr e2) then TY_Bool
        else (error "Type mismatch (e1 = e2)"; raise Terminate)
      else (error "Type mismatch (e1 =) array-function type"; raise Terminate)
  | E_Differ (e1, e2)      ->
      let typ1 = typeOfExpr e1 in
      if isNotArrayFunc typ1 then
        if (=) typ1 (typeOfExpr e2) then TY_Bool
        else (error "Type mismatch (e1 <> e2)"; raise Terminate)
      else (error "Type mismatch (e1 <>) array-function type"; raise Terminate)
  | E_Equal (e1, e2)       ->
      let typ1 = typeOfExpr e1 in
      if isNotArrayFunc typ1 then
        if (=) typ1 (typeOfExpr e2) then TY_Bool
        else (error "Type mismatch (e1 == e2)"; raise Terminate)
      else (error "Type mismatch (e1 ==) array-function type"; raise Terminate)
  | E_NEqual (e1, e2)      ->
      let typ1 = typeOfExpr e1 in
      if isNotArrayFunc typ1 then
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
