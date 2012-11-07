(* Main Intermediate functions:
 * traverses the ast, informs the appropriate variables and structs and
 * generates the quadruples *)

open InterUtils

module I = InterUtils

open Ast
open Error

let rec interOf = function
    PROGRAM (ldfs, tdfs) ->
      List.iter (fun x -> interOfLetdef x) ldfs;
      List.iter (fun x -> interOfTypedef x) tdfs

and interOfLetdef = function
    L_Let (sem, fi, vl)    -> () (* vl: vardefs connected with 'and' keyword *)
  | L_LetRec (sem, fi, vl) -> () (* vl: vardefs connected with 'and' keyword *)
and interOfTypedef = function
    TD_Type (sem, fi, tl)       ->  (* TODO: Not supported yet *)
      error fi 3 "user defined data types are not supported"
  | TD_TDefId (sem, fi, s, tl)  ->  (* TODO: Not supported yet *)
      error fi 3 "user defined data types are not supported"
  | TD_Constr (sem, fi, s, tyl) ->  (* TODO: Not supported yet *)
      error fi 3 "user defined data types are not supported"

and interOfVardef rec_flag = function
    VAR_Id (sem, fi, s, varl, e) -> sem
  | VAR_MutId (sem, fi, s, exprl) -> sem

and interOfExpr = function
  (* Constants Operators *)
    E_Unit (sem, info)        ->
      { sem with val_type = I.Rval; place = I.Unit }
  | E_True (sem, info)        ->
      { sem with val_type = I.Rval; place = I.True }
  | E_False (sem, info)       ->
      { sem with val_type = I.Rval; place = I.False }
  | E_LitInt (sem, info, i)   ->
      { sem with val_type = I.Rval; place = I.Int i }
  | E_LitChar (sem, info, c)  ->
      { sem with val_type = I.Rval; place = I.Char c }
  | E_LitFloat (sem, info, f) ->
      { sem with val_type = I.Rval; place = I.Float f }
  | E_LitString (sem, fi, s)  ->
      { sem with val_type = I.Rval; place = I.String s }
  (* Names (constants, functions, parameters, constructors, expressions) *)
  | E_LitId (sem, fi, id)    -> sem
  | E_LitConstr (sem, fi, id) -> (* TODO: not supported yet *)
      error fi 3 "user defined date types are not supported"
  (* Unary Arithmetic Operators *)
  | E_UPlus   (sem, fi, e)     ->
      { sem with place = (interOfExpr e).place }
  | E_UFPlus  (sem, fi, e)    ->
      { sem with place = (interOfExpr e).place }
  | E_UMinus  (sem, fi, e)    ->
      let w = I.Entry (newTemp fi sem.expr_type) in
      let q = genQuad I.O_Minus (interOfExpr e).place I.Empty w in
      add_quad q;
      { sem with place = w }
  | E_UFMinus (sem, fi, e)   ->
      let w = I.Entry (newTemp fi sem.expr_type) in
      let q = genQuad I.O_Minus (interOfExpr e).place I.Empty w in
      add_quad q;
      { sem with place = w }
  (* References and assigments *)
  | E_Assign (sem, fi, e1, e2)      -> sem (* FIXME: check if e1 is l-value *)
  | E_Deref  (sem, fi, e)     -> sem
  (* Memory Dynamic Allocation *)
  | E_New    (sem, fi)         -> sem
  | E_Delete (sem, fi, e)      -> sem (* FIXME: check that mem was allocated dynamically *)
  (* Binary Integer Arithmetic Operators *)
  | E_Plus  (sem, fi, e1, e2)   ->
      let w = I.Entry (newTemp fi sem.expr_type) in
      let q = genQuad I.O_Plus (interOfExpr e1).place (interOfExpr e2).place w
      in
      add_quad q;
      { sem with place = w }
  | E_Minus (sem, fi, e1, e2)  ->
      let w = I.Entry (newTemp fi sem.expr_type) in
      let q = genQuad I.O_Minus (interOfExpr e1).place (interOfExpr e2).place w
      in
      add_quad q;
      { sem with place = w }
  | E_Mul   (sem, fi, e1, e2)    ->
      let w = I.Entry (newTemp fi sem.expr_type) in
      let q = genQuad I.O_Mult (interOfExpr e1).place (interOfExpr e2).place w
      in
      add_quad q;
      { sem with place = w }
  | E_Div   (sem, fi, e1, e2)    ->
      let w = I.Entry (newTemp fi sem.expr_type) in
      let q = genQuad I.O_Div (interOfExpr e1).place (interOfExpr e2).place w
      in
      add_quad q;
      { sem with place = w }
  | E_Mod   (sem, fi, e1, e2)     ->
      let w = I.Entry (newTemp fi sem.expr_type) in
      let q = genQuad I.O_Mod (interOfExpr e1).place (interOfExpr e2).place w
      in
      add_quad q;
      { sem with place = w }
  (* Binary Float Arithmetic Operators *)
  | E_FPlus (sem, fi, e1, e2)  ->
      let w = I.Entry (newTemp fi sem.expr_type) in
      let q = genQuad I.O_Plus (interOfExpr e1).place (interOfExpr e2).place w
      in
      add_quad q;
      { sem with place = w }
  | E_FMinus (sem, fi, e1, e2) ->
      let w = I.Entry (newTemp fi sem.expr_type) in
      let q = genQuad I.O_Minus (interOfExpr e1).place (interOfExpr e2).place w
      in
      add_quad q;
      { sem with place = w }
  | E_FMul (sem, fi, e1, e2)   ->
      let w = I.Entry (newTemp fi sem.expr_type) in
      let q = genQuad I.O_Mult (interOfExpr e1).place (interOfExpr e2).place w
      in
      add_quad q;
      { sem with place = w }
  | E_FDiv (sem, fi, e1, e2)   ->
      let w = I.Entry (newTemp fi sem.expr_type) in
      let q = genQuad I.O_Div (interOfExpr e1).place (interOfExpr e2).place w
      in
      add_quad q;
      { sem with place = w }
  | E_Pow (sem, fi, e1, e2)    ->
      let w = I.Entry (newTemp fi sem.expr_type) in
      let q = genQuad I.O_Pow (interOfExpr e1).place (interOfExpr e2).place w
      in
      add_quad q;
      { sem with place = w }
  (* Structural and Natural Equality Operators *)
  | E_Eq (sem, fi, e1, e2)       ->
      let quad_true = nextQuad () in
      let q = genQuad I.O_SEqual (interOfExpr e1).place (interOfExpr e2).place
                      I.Backpatch in
      add_quad q;
      let quad_false = nextQuad () in
      let q = genQuad I.O_Jump I.Empty I.Empty I.Backpatch in
      add_quad q;
      { sem with true_ = [quad_true]; false_ = [quad_false] }
  | E_Differ (sem, fi, e1, e2)   ->
      let quad_true = nextQuad () in
      let q = genQuad I.O_SNEqual (interOfExpr e1).place (interOfExpr e2).place
                      I.Backpatch in
      add_quad q;
      let quad_false = nextQuad () in
      let q = genQuad I.O_Jump I.Empty I.Empty I.Backpatch in
      add_quad q;
      { sem with true_ = [quad_true]; false_ = [quad_false] }
  | E_Equal (sem, fi, e1, e2)    ->
      let quad_true = nextQuad () in
      let q = genQuad I.O_Equal (interOfExpr e1).place (interOfExpr e2).place
                      I.Backpatch in
      add_quad q;
      let quad_false = nextQuad () in
      let q = genQuad I.O_Jump I.Empty I.Empty I.Backpatch in
      add_quad q;
      { sem with true_ = [quad_true]; false_ = [quad_false] }
  | E_NEqual (sem, fi, e1, e2)   ->
      let quad_true = nextQuad () in
      let q = genQuad I.O_NEqual (interOfExpr e1).place (interOfExpr e2).place
                      I.Backpatch in
      add_quad q;
      let quad_false = nextQuad () in
      let q = genQuad I.O_Jump I.Empty I.Empty I.Backpatch in
      add_quad q;
      { sem with true_ = [quad_true]; false_ = [quad_false] }
  | E_Lt (sem, fi, e1, e2)       ->
      let quad_true = nextQuad () in
      let q = genQuad I.O_Lt (interOfExpr e1).place (interOfExpr e2).place
                      I.Backpatch in
      add_quad q;
      let quad_false = nextQuad () in
      let q = genQuad I.O_Jump I.Empty I.Empty I.Backpatch in
      add_quad q;
      { sem with true_ = [quad_true]; false_ = [quad_false] }
  | E_Gt (sem, fi, e1, e2)       ->
      let quad_true = nextQuad () in
      let q = genQuad I.O_Gt (interOfExpr e1).place (interOfExpr e2).place
                      I.Backpatch in
      add_quad q;
      let quad_false = nextQuad () in
      let q = genQuad I.O_Jump I.Empty I.Empty I.Backpatch in
      add_quad q;
      { sem with true_ = [quad_true]; false_ = [quad_false] }
  | E_Leq (sem, fi, e1, e2)      ->
      let quad_true = nextQuad () in
      let q = genQuad I.O_Leq (interOfExpr e1).place (interOfExpr e2).place
                      I.Backpatch in
      add_quad q;
      let quad_false = nextQuad () in
      let q = genQuad I.O_Jump I.Empty I.Empty I.Backpatch in
      add_quad q;
      { sem with true_ = [quad_true]; false_ = [quad_false] }
  | E_Geq (sem, fi, e1, e2)      ->
      let quad_true = nextQuad () in
      let q = genQuad I.O_Geq (interOfExpr e1).place (interOfExpr e2).place
                      I.Backpatch in
      add_quad q;
      let quad_false = nextQuad () in
      let q = genQuad I.O_Jump I.Empty I.Empty I.Backpatch in
      add_quad q;
      { sem with true_ = [quad_true]; false_ = [quad_false] }
  (* Logical Operators *)
  | E_Not (sem, fi, e)           ->
      let inter_e = interOfExpr e in
      { sem with true_ = inter_e.false_; false_ = inter_e.true_ }
  | E_Andlogic (sem, fi, e1, e2) ->
      backpatch (interOfExpr e1).true_ (nextQuad ());
      { sem with
          true_ = (interOfExpr e2).true_;
          false_ = merge [(interOfExpr e1).false_; (interOfExpr e2).false_] }
  | E_Orlogic (sem, fi, e1, e2)  ->
      backpatch (interOfExpr e1).false_ (nextQuad ());
      { sem with
          true_ = merge [(interOfExpr e1).true_; (interOfExpr e2).true_];
          false_ = (interOfExpr e2).false_ }
  (* Imperative Commands *)
  | E_Block (sem, fi, e)     -> sem
  | E_Semicolon (sem, fi, e1, e2)   -> sem
  | E_While (sem, fi, e1, e2)       -> sem
  | E_For (sem, fi, s, ti, e1, e2, e)  -> sem
  (* Decomposition of User Defined Types *)
  | E_Match (sem, fi, e, clauses)   ->
      error fi 3 "user defined data types are not supported"
  (* Local definitions *)
  | E_LetIn (sem, fi, ld, e)        -> sem (* local declarations *)
  (* If statement *)
  | E_IfStmt (sem, fi, e, e1, _e2)   -> sem
  (* Array Elements and Dimensions *)
  | E_Dim (sem, fi, i, s)           -> sem
  | E_ArrayEl (sem, fi, s, el, el_len)      -> sem
  (* Function and Constructor call *)
  | E_Call (sem, fi, s, el)         -> sem
  | E_ConstrCall (sem, fi, s, el)  ->  (* TODO: not supported yet *)
      error fi 3 "user defined data types are not supported"


and interOfPattern = function
    P_True (sem, info)             -> sem
  | P_False (sem, info)            -> sem
  | P_LitId (sem, fi,id)           -> sem
  | P_LitChar (sem, fi, c)         -> sem
  | P_LitFloat (sem, fi, f)        -> sem
  | P_Plus (sem, fi, _)            -> sem
  | P_FPlus (sem, fi, _)           -> sem
  | P_Minus (sem, fi, _)           -> sem
  | P_FMinus  (sem, fi, _)         -> sem
  | P_LitConstr (sem, fi, s, patl) -> (* TODO: not supported yet *)
      error fi 3 "user defined data types are not supported"
  | _                 -> err "Wrong pattern form"
;;
