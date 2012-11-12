(* Main Intermediate functions:
 * traverses the ast, informs the appropriate variables and structs and
 * generates the quadruples *)

open InterUtils

module I = InterUtils

open Ast
open Error
open Symbol
open Types
open Identifier

(* prints the entry attributes of the sem_val given *)
let pp_print sem = 
  Printf.printf "error: %s, %s\n" (id_name sem.entry.entry_id)
  (str_of_entry_info sem.entry.entry_info);
  pretty_type Format.std_formatter sem.expr_type;
  Format.print_newline()

let rec interOf = function
    PROGRAM (ldfs, tdfs) ->
      List.iter (fun x -> interOfLetdef x) ldfs;
      List.iter (fun x -> interOfTypedef x) tdfs

and interOfLetdef = function
  (* vl: vardefs connected with 'and' keyword *)
    L_Let (sem, fi, vl)    ->
      let printer v = 
        Printf.printf "L_Let: %s, %d, %s\n" (id_name v.entry_id) 
        v.entry_scope.sco_nesting (str_of_entry_info v.entry_info)
      in
      List.iter (fun x -> printer (interOfVardef false x).entry) vl
  | L_LetRec (sem, fi, vl) -> ()

and interOfTypedef = function
    TD_Type (sem, fi, tl)    ->  (* TODO: Not supported yet *)
      error fi 3 "user defined data types are not supported"
  | TD_TDefId (sem, fi, tl)  ->  (* TODO: Not supported yet *)
      error fi 3 "user defined data types are not supported"
  | TD_Constr (sem, fi, tyl) ->  (* TODO: Not supported yet *)
      error fi 3 "user defined data types are not supported"

and interOfVardef rec_flag = function
    VAR_Id (sem, fi, varl, e) -> interOfExpr e
  | VAR_MutId (sem, fi, exprl) -> sem

and interOfExpr = function
  (* Constants Operators *)
    E_Unit (sem, info)    ->
      sem.place <- I.Unit; sem
  | E_True (sem, info)    ->
      sem.place <- I.True; sem
  | E_False (sem, info)   ->
      sem.place <- I.False; sem
  | E_LitInt (sem, info, i)   ->
      sem.place <- I.Int i; sem
  | E_LitChar (sem, info, c)  ->
      sem.place <- I.Char c; sem
  | E_LitFloat (sem, info, f) ->
      sem.place <- I.Float f; sem
  | E_LitString (sem, fi, s)  ->
      sem.place <- I.String s; sem
  (* Names (constants, functions, parameters, constructors, expressions) *)
  | E_LitId (sem, fi)    ->
      begin (* FIXME: what about ENTRY_FUNCTION call, Lval check *)
        match sem.entry.entry_info with
        | ENTRY_parameter _ | ENTRY_variable _ ->
            sem.place <- I.Entry sem.entry; sem
        | ENTRY_function _ -> raise (Exit 4)
        | _ -> raise Terminate
      end
  | E_LitConstr (sem, fi) -> (* TODO: not supported yet *)
      error fi 3 "user defined date types are not supported"
  (* Unary Arithmetic Operators *)
  | E_UPlus   (sem, fi, e)     ->
      sem.place <- (interOfExpr e).place; sem
  | E_UFPlus  (sem, fi, e)    ->
      sem.place <- (interOfExpr e).place; sem
  | E_UMinus  (sem, fi, e)    ->
      let w = I.Entry (newTemp fi sem.expr_type) in
      let q = genQuad I.O_Minus (interOfExpr e).place I.Empty w in
      add_quad q;
      sem.place <- w;
      sem
  | E_UFMinus (sem, fi, e)   ->
      let w = I.Entry (newTemp fi sem.expr_type) in
      let q = genQuad I.O_Minus (interOfExpr e).place I.Empty w in
      add_quad q;
      sem.place <- w;
      sem
  (* References and assigments *)
  | E_Assign (sem, fi, e1, e2)  ->
      let q = genQuad I.O_Assign (interOfExpr e2).place I.Empty
              (interOfExpr e1).place in
      add_quad q;
      sem.next <- [];
      sem
  | E_Deref  (sem, fi, e)       -> (* FIXME: type should be ref? *)
      let w = newTemp fi sem.expr_type in
      let q = genQuad I.O_Assign (interOfExpr e).place I.Empty (I.Entry w) in
      add_quad q;
      sem.place <- I.Pointer (w, sem.expr_type);
      sem
  (* FIXME: Memory Dynamic Allocation *)
  | E_New    (sem, fi)          ->
      let size = sizeOfType sem.expr_type in
      add_quad (genQuad I.O_Par (I.Int size) (I.Pass V) I.Empty);
      add_quad (genQuad I.O_Par sem.place (I.Pass RET) I.Empty);
      add_quad (genQuad I.O_Call I.Empty I.Empty I.New);
      sem.next <- [];
      sem
  | E_Delete (sem, fi, e)       -> (* FIXME: check that mem was allocated dynamically *)
      add_quad (genQuad I.O_Par (interOfExpr e).place (I.Pass R) I.Empty);
      add_quad (genQuad I.O_Call I.Empty I.Empty I.Delete);
      sem.next <- [];
      sem
  (* Binary Integer Arithmetic Operators *)
  | E_Plus  (sem, fi, e1, e2)   ->
      let w = I.Entry (newTemp fi sem.expr_type) in
      let q = genQuad I.O_Plus (interOfExpr e1).place (interOfExpr e2).place w
      in
      add_quad q;
      sem.place <- w;
      sem
  | E_Minus (sem, fi, e1, e2)  ->
      let w = I.Entry (newTemp fi sem.expr_type) in
      let q = genQuad I.O_Minus (interOfExpr e1).place (interOfExpr e2).place w
      in
      add_quad q;
      sem.place <- w;
      sem
  | E_Mul   (sem, fi, e1, e2)    ->
      let w = I.Entry (newTemp fi sem.expr_type) in
      let q = genQuad I.O_Mult (interOfExpr e1).place (interOfExpr e2).place w
      in
      add_quad q;
      sem.place <- w;
      sem
  | E_Div   (sem, fi, e1, e2)    ->
      let w = I.Entry (newTemp fi sem.expr_type) in
      let q = genQuad I.O_Div (interOfExpr e1).place (interOfExpr e2).place w
      in
      add_quad q;
      sem.place <- w;
      sem
  | E_Mod   (sem, fi, e1, e2)     ->
      let w = I.Entry (newTemp fi sem.expr_type) in
      let q = genQuad I.O_Mod (interOfExpr e1).place (interOfExpr e2).place w
      in
      add_quad q;
      sem.place <- w;
      sem
  (* Binary Float Arithmetic Operators *)
  | E_FPlus (sem, fi, e1, e2)  ->
      let w = I.Entry (newTemp fi sem.expr_type) in
      let q = genQuad I.O_Plus (interOfExpr e1).place (interOfExpr e2).place w
      in
      add_quad q;
      sem.place <- w;
      sem
  | E_FMinus (sem, fi, e1, e2) ->
      let w = I.Entry (newTemp fi sem.expr_type) in
      let q = genQuad I.O_Minus (interOfExpr e1).place (interOfExpr e2).place w
      in
      add_quad q;
      sem.place <- w;
      sem
  | E_FMul (sem, fi, e1, e2)   ->
      let w = I.Entry (newTemp fi sem.expr_type) in
      let q = genQuad I.O_Mult (interOfExpr e1).place (interOfExpr e2).place w
      in
      add_quad q;
      sem.place <- w;
      sem
  | E_FDiv (sem, fi, e1, e2)   ->
      let w = I.Entry (newTemp fi sem.expr_type) in
      let q = genQuad I.O_Div (interOfExpr e1).place (interOfExpr e2).place w
      in
      add_quad q;
      sem.place <- w;
      sem
  | E_Pow (sem, fi, e1, e2)    ->
      let w = I.Entry (newTemp fi sem.expr_type) in
      let q = genQuad I.O_Pow (interOfExpr e1).place (interOfExpr e2).place w
      in
      add_quad q;
      sem.place <- w;
      sem
  (* Structural and Natural Equality Operators *)
  | E_Eq (sem, fi, e1, e2)       ->
      let quad_true = nextQuad () in
      let q = genQuad I.O_SEqual (interOfExpr e1).place (interOfExpr e2).place
                      I.Backpatch in
      add_quad q;
      let quad_false = nextQuad () in
      let q = genQuad I.O_Jump I.Empty I.Empty I.Backpatch in
      add_quad q;
      sem.true_ <- [quad_true]; 
      sem.false_ <- [quad_false];
      sem
  | E_Differ (sem, fi, e1, e2)   ->
      let quad_true = nextQuad () in
      let q = genQuad I.O_SNEqual (interOfExpr e1).place (interOfExpr e2).place
                      I.Backpatch in
      add_quad q;
      let quad_false = nextQuad () in
      let q = genQuad I.O_Jump I.Empty I.Empty I.Backpatch in
      add_quad q;
      sem.true_ <- [quad_true]; 
      sem.false_ <- [quad_false];
      sem
  | E_Equal (sem, fi, e1, e2)    ->
      let quad_true = nextQuad () in
      let q = genQuad I.O_Equal (interOfExpr e1).place (interOfExpr e2).place
                      I.Backpatch in
      add_quad q;
      let quad_false = nextQuad () in
      let q = genQuad I.O_Jump I.Empty I.Empty I.Backpatch in
      add_quad q;
      sem.true_ <- [quad_true]; 
      sem.false_ <- [quad_false];
      sem
  | E_NEqual (sem, fi, e1, e2)   ->
      let quad_true = nextQuad () in
      let q = genQuad I.O_NEqual (interOfExpr e1).place (interOfExpr e2).place
                      I.Backpatch in
      add_quad q;
      let quad_false = nextQuad () in
      let q = genQuad I.O_Jump I.Empty I.Empty I.Backpatch in
      add_quad q;
      sem.true_ <- [quad_true]; 
      sem.false_ <- [quad_false];
      sem
  | E_Lt (sem, fi, e1, e2)       ->
      let quad_true = nextQuad () in
      let q = genQuad I.O_Lt (interOfExpr e1).place (interOfExpr e2).place
                      I.Backpatch in
      add_quad q;
      let quad_false = nextQuad () in
      let q = genQuad I.O_Jump I.Empty I.Empty I.Backpatch in
      add_quad q;
      sem.true_ <- [quad_true]; 
      sem.false_ <- [quad_false];
      sem
  | E_Gt (sem, fi, e1, e2)       ->
      let quad_true = nextQuad () in
      let q = genQuad I.O_Gt (interOfExpr e1).place (interOfExpr e2).place
                      I.Backpatch in
      add_quad q;
      let quad_false = nextQuad () in
      let q = genQuad I.O_Jump I.Empty I.Empty I.Backpatch in
      add_quad q;
      sem.true_ <- [quad_true]; 
      sem.false_ <- [quad_false];
      sem
  | E_Leq (sem, fi, e1, e2)      ->
      let quad_true = nextQuad () in
      let q = genQuad I.O_Leq (interOfExpr e1).place (interOfExpr e2).place
                      I.Backpatch in
      add_quad q;
      let quad_false = nextQuad () in
      let q = genQuad I.O_Jump I.Empty I.Empty I.Backpatch in
      add_quad q;
      sem.true_ <- [quad_true]; 
      sem.false_ <- [quad_false];
      sem
  | E_Geq (sem, fi, e1, e2)      ->
      let quad_true = nextQuad () in
      let q = genQuad I.O_Geq (interOfExpr e1).place (interOfExpr e2).place
                      I.Backpatch in
      add_quad q;
      let quad_false = nextQuad () in
      let q = genQuad I.O_Jump I.Empty I.Empty I.Backpatch in
      add_quad q;
      sem.true_ <- [quad_true]; 
      sem.false_ <- [quad_false];
      sem
  (* Logical Operators *)
  | E_Not (sem, fi, e)           ->
      let inter_e = interOfExpr e in
      sem.true_ <- inter_e.false_; 
      sem.false_ <- inter_e.true_;
      sem 
  | E_Andlogic (sem, fi, e1, e2) ->
      backpatch (interOfExpr e1).true_ (nextQuad ());
      sem.true_ <- (interOfExpr e2).true_;
      sem.false_ <- merge [(interOfExpr e1).false_; (interOfExpr e2).false_];
      sem
  | E_Orlogic (sem, fi, e1, e2)  ->
      backpatch (interOfExpr e1).false_ (nextQuad ());
      sem.true_ <- merge [(interOfExpr e1).true_; (interOfExpr e2).true_];
      sem.false_ <- (interOfExpr e2).false_;
      sem
  (* Imperative Commands *)
  | E_Block (sem, fi, e)     -> sem
  | E_Semicolon (sem, fi, e1, e2)  -> sem
  | E_While (sem, fi, e1, e2)      ->
      let q = nextQuad () in
      backpatch (interOfExpr e1).true_ (nextQuad ());
      backpatch (interOfExpr e2).next q;
      let quad = genQuad I.O_Jump I.Empty I.Empty (I.Label q) in
      add_quad quad;
      sem.next <- (interOfExpr e1).false_;
      sem
  | E_For (sem, fi,ti, e1, e2, e) -> (* FIXME: is it correct? *)
      let cond_q = nextQuad ()
      and cond_true = (interOfExpr e2).true_
      and cond_false = (interOfExpr e2).false_
      and simple_quad = nextQuad ()
      and body_fst_quad = nextQuad () + 1 in
      let q1 = genQuad I.O_Jump I.Empty I.Empty (I.Label cond_q) in
      add_quad q1;
      backpatch cond_true body_fst_quad;
      let q2 = genQuad I.O_Jump I.Empty I.Empty (I.Label simple_quad) in
      add_quad q2;
      sem.next <- cond_false;
      sem
  (* Decomposition of User Defined Types *)
  | E_Match (sem, fi, e, clauses)   ->
      error fi 3 "user defined data types are not supported"
  (* Local definitions *)
  | E_LetIn (sem, fi, ld, e)        -> sem (* local declarations *)
  (* If statement *)
  | E_IfStmt (sem, fi, e, e1, _e2)  ->
      let cond_sem = interOfExpr e in
      let stmt1_sem = interOfExpr e1 in
      backpatch cond_sem.true_ (nextQuad ());
      let l1 = cond_sem.false_ in
      let l2 = [] in begin
      match _e2 with
      | Some e2 -> (* else stmt *)
          let stmt2_sem = interOfExpr e2 in
          let l1 = [nextQuad ()] in
          let q = genQuad I.O_Jump I.Empty I.Empty I.Backpatch in
          add_quad q;
          backpatch cond_sem.false_ (nextQuad ());
          let l2 = stmt2_sem.next in
          sem.next <- merge [l1; stmt1_sem.next; l2];
          sem
      | None    -> (* no else stmt *)
          sem.next <- merge [l1; stmt1_sem.next; l2];
          sem
      end
  (* Array Elements and Dimensions *)
  | E_Dim (sem, fi, i)      -> sem
  | E_ArrayEl (sem, fi, el) -> sem
  (* Function and Constructor call *)
  | E_Call (sem, fi, el)    -> sem
  | E_ConstrCall (sem, fi,el) ->  (* TODO: not supported yet *)
      error fi 3 "user defined data types are not supported"


and interOfPattern = function
    P_True (sem, fi)            -> sem
  | P_False (sem, fi)           -> sem
  | P_LitId (sem, fi)        -> sem
  | P_LitChar (sem, fi, c)      -> sem
  | P_LitFloat (sem, fi, f)     -> sem
  | P_Plus (sem, fi, _)         -> sem
  | P_FPlus (sem, fi, _)        -> sem
  | P_Minus (sem, fi, _)        -> sem
  | P_FMinus  (sem, fi, _)      -> sem
  | P_LitConstr (sem, fi, patl) -> (* TODO: not supported yet *)
      error fi 3 "user defined data types are not supported"
  | _                 -> err "Wrong pattern form"
;;
