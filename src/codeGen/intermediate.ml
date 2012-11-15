(* Main Intermediate functions:
 * traverses the ast, informs the appropriate variables and structs and
 * generates the quadruples *)

open Printf

open InterUtils

module I = InterUtils

open Ast
open Error
open Symbol
open Types
open Identifier

(* Used for getting the return stmt of a function:
 * we put in it all the sem_val structs of the valued expressions. The last
 * valued expression putted in before creating the final function quad, that is
 * <endu, name, _, _>, is the return function's stmt. *)
let func_res = ref []

let rec interOf = function
    PROGRAM (ldfs, tdfs) ->
      add_quad (genQuad I.O_Unit (I.String "_outer") I.Empty I.Empty);
      List.iter (fun x -> interOfLetdef x) ldfs;
      (*backpatch (List.hd !func_res).next (nextQuad ());*)
      add_quad (genQuad I.O_Endu (I.String "_outer") I.Empty I.Empty);
      List.iter (fun x -> interOfTypedef x) tdfs

and interOfLetdef = function
  (* vl: vardefs connected with 'and' keyword *)
    L_Let (sem, fi, vl)    ->
      let var_or_func v =
        match v with
        (* variable definition *)
        | VAR_Id (sem, fi, [], _) as x ->
            Pervasives.ignore (interOfVardef x)
        (* function definition *)
        | VAR_Id (sem, _, lst, _) as x ->
            Pervasives.ignore (interOfVardef x);
            let name = id_name sem.entry.entry_id in
            if sem.place = I.Invalid
            then add_quad (genQuad I.O_Assign (List.hd !func_res).place I.Empty
                                  (I.Result sem.expr_type))
            else add_quad (genQuad I.O_Assign sem.place I.Empty
                                  (I.Result sem.expr_type));
            (*backpatch sem.next (nextQuad ());*)
            add_quad (genQuad I.O_Endu (I.String name) I.Empty I.Empty)
        | _ -> error fi 4 "not var or func type"
      in List.iter (fun x -> var_or_func x) vl
  | L_LetRec (sem, fi, vl) -> ()

and interOfTypedef = function
    TD_Type (sem, fi, tl)    ->  (* TODO: Not supported yet *)
      error fi 3 "user defined data types are not supported"
  | TD_TDefId (sem, fi, tl)  ->  (* TODO: Not supported yet *)
      error fi 3 "user defined data types are not supported"
  | TD_Constr (sem, fi, tyl) ->  (* TODO: Not supported yet *)
      error fi 3 "user defined data types are not supported"

and interOfVardef = function
    VAR_Id (sem, fi, varl, e) ->
      begin
        match sem.entry.entry_info with
        | ENTRY_variable _ ->
            let inter_e = interOfExpr e in
            if sem.val_type <> Cond
            then begin
              if inter_e.place = I.Invalid then ()
              else add_quad (genQuad I.O_Assign inter_e.place I.Empty
                                  (I.Entry sem.entry))
            end
            else begin
              let w = I.Entry (newTemp fi sem.expr_type) in
              backpatch inter_e.true_ (nextQuad ());
              add_quad (genQuad I.O_Assign I.True I.Empty w);
              add_quad (genQuad I.O_Jump I.Empty I.Empty
                               (I.Label (nextQuad () + 2)));
              backpatch inter_e.false_ (nextQuad ());
              add_quad (genQuad I.O_Assign I.False I.Empty w);
              sem.place <- w;
              add_quad (genQuad I.O_Assign sem.place I.Empty
                                 (I.Entry sem.entry))
            end;
            sem
        | ENTRY_function _ ->
            let name = id_name sem.entry.entry_id in
            add_quad (genQuad I.O_Unit (I.String name) I.Empty I.Empty);
            let inter_e = interOfExpr e in
            if sem.val_type <> Cond then printf "Wrong\n"
            else begin
              let w = I.Entry (newTemp fi sem.expr_type) in
              let q = nextQuad () in
              backpatch inter_e.true_ q;
              add_quad (genQuad I.O_Assign I.True I.Empty w);
              add_quad (genQuad I.O_Jump I.Empty I.Empty
                               (I.Label (nextQuad () + 2)));
              backpatch inter_e.false_ (nextQuad ());
              add_quad (genQuad I.O_Assign I.False I.Empty w);
              sem.place <- w
            end;
            sem
        | _ -> error fi 4 "wrong file info"
      end
  | VAR_MutId (sem, fi, exprl) -> sem

and interOfExpr = function
  (* Constants Operators *)
    E_Unit (sem, info)    ->
      sem.place <- I.Unit;
      func_res := sem :: !func_res;
      sem
  | E_True (sem, info)    ->
      if sem.val_type <> I.Cond then printf "wrong\n"
      else (sem.true_ <- [nextQuad ()];
      add_quad (genQuad I.O_Jump I.Empty I.Empty I.Backpatch));
      sem.place <- I.True;
      func_res := sem :: !func_res;
      sem
  | E_False (sem, info)   ->
      if sem.val_type <> I.Cond then printf "wrong:false\n"
      else (sem.false_ <- [nextQuad ()];
      add_quad (genQuad I.O_Jump I.Empty I.Empty I.Backpatch));
      sem.place <- I.False;
      func_res := sem :: !func_res;
      sem
  | E_LitInt (sem, info, i)   ->
      sem.place <- I.Int i;
      func_res := sem :: !func_res;
      sem
  | E_LitChar (sem, info, c)  ->
      sem.place <- I.Char c;
      func_res := sem :: !func_res;
      sem
  | E_LitFloat (sem, info, f) ->
      sem.place <- I.Float f;
      func_res := sem :: !func_res;
      sem
  | E_LitString (sem, fi, s)  ->
      sem.place <- I.String s;
      func_res := sem :: !func_res;
      sem
  (* Names (constants, functions, parameters, constructors, expressions) *)
  | E_LitId (sem, fi)    ->
      begin (* FIXME: what about ENTRY_FUNCTION call, Lval check *)
        match sem.entry.entry_info with
        | ENTRY_parameter _ | ENTRY_variable _ ->
            sem.place <- I.Entry sem.entry;
            if sem.val_type <> Cond then ()
            else begin
              sem.true_ <- [nextQuad ()];
              add_quad (genQuad I.O_Ifjump sem.place I.Empty I.Backpatch);
              sem.false_ <- [nextQuad ()];
              add_quad (genQuad I.O_Jump I.Empty I.Empty I.Backpatch)
            end;
            func_res := sem :: !func_res;
            sem
        | ENTRY_function _ -> raise (Exit 4)
        | _ -> raise Terminate
      end
  | E_LitConstr (sem, fi) -> (* TODO: not supported yet *)
      error fi 3 "user defined date types are not supported"
  (* Unary Arithmetic Operators *)
  | E_UPlus   (sem, fi, e)     ->
      sem.place <- (interOfExpr e).place;
      func_res := sem :: !func_res;
      sem
  | E_UFPlus  (sem, fi, e)    ->
      sem.place <- (interOfExpr e).place;
      func_res := sem :: !func_res;
      sem
  | E_UMinus  (sem, fi, e)    ->
      let w = I.Entry (newTemp fi sem.expr_type) in
      add_quad (genQuad I.O_Minus (interOfExpr e).place I.Empty w);
      sem.place <- w;
      func_res := sem :: !func_res;
      sem
  | E_UFMinus (sem, fi, e)   ->
      let w = I.Entry (newTemp fi sem.expr_type) in
      add_quad (genQuad I.O_Minus (interOfExpr e).place I.Empty w);
      sem.place <- w;
      func_res := sem :: !func_res;
      sem
  (* References and assigments *)
  | E_Assign (sem, fi, e1, e2)  ->
      let sem1 = interOfExpr e1
      and sem2 = interOfExpr e2 in
      sem1.place <- I.Pointer (sem1.entry, sem1.expr_type);
      add_quad (genQuad I.O_Assign sem2.place I.Empty sem1.place);
      sem.next <- [];
      func_res := sem :: !func_res;
      sem
  | E_Deref  (sem, fi, e)       -> (* FIXME: type should be ref? *)
      let w = newTemp fi sem.expr_type in
      add_quad (genQuad I.O_Assign (interOfExpr e).place I.Empty (I.Entry w));
      sem.place <- I.Pointer (w, sem.expr_type);
      func_res := sem :: !func_res;
      sem
  (* FIXME: Memory Dynamic Allocation *)
  | E_New    (sem, fi)          ->
      let size = sizeOfType sem.expr_type in
      add_quad (genQuad I.O_Par (I.Int size) (I.Pass V) I.Empty);
      add_quad (genQuad I.O_Par sem.place (I.Pass RET) I.Empty);
      add_quad (genQuad I.O_Call I.Empty I.Empty I.New);
      sem.next <- [];
      func_res := sem :: !func_res;
      sem
  | E_Delete (sem, fi, e)       -> (* FIXME: check that mem was allocated dynamically *)
      add_quad (genQuad I.O_Par (interOfExpr e).place (I.Pass R) I.Empty);
      add_quad (genQuad I.O_Call I.Empty I.Empty I.Delete);
      sem.next <- [];
      func_res := sem :: !func_res;
      sem
  (* Binary Integer Arithmetic Operators *)
  | E_Plus  (sem, fi, e1, e2)   ->
      let w = I.Entry (newTemp fi sem.expr_type)
      and sem1 = interOfExpr e1
      and sem2 = interOfExpr e2 in
      add_quad (genQuad I.O_Plus sem1.place sem2.place w);
      sem.place <- w;
      func_res := sem :: !func_res;
      sem
  | E_Minus (sem, fi, e1, e2)  ->
      let w = I.Entry (newTemp fi sem.expr_type)
      and sem1 = interOfExpr e1
      and sem2 = interOfExpr e2 in
      add_quad (genQuad I.O_Minus sem1.place sem2.place w);
      sem.place <- w;
      func_res := sem :: !func_res;
      sem
  | E_Mul   (sem, fi, e1, e2)    ->
      let w = I.Entry (newTemp fi sem.expr_type)
      and sem1 = interOfExpr e1
      and sem2 = interOfExpr e2 in
      add_quad (genQuad I.O_Mult sem1.place sem2.place w);
      sem.place <- w;
      func_res := sem :: !func_res;
      sem
  | E_Div   (sem, fi, e1, e2)    ->
      let w = I.Entry (newTemp fi sem.expr_type)
      and sem1 = interOfExpr e1
      and sem2 = interOfExpr e2 in
      add_quad (genQuad I.O_Div sem1.place sem2.place w);
      sem.place <- w;
      func_res := sem :: !func_res;
      sem
  | E_Mod   (sem, fi, e1, e2)     ->
      let w = I.Entry (newTemp fi sem.expr_type)
      and sem1 = interOfExpr e1
      and sem2 = interOfExpr e2 in
      add_quad (genQuad I.O_Mod sem1.place sem2.place w);
      sem.place <- w;
      func_res := sem :: !func_res;
      sem
  (* Binary Float Arithmetic Operators *)
  | E_FPlus (sem, fi, e1, e2)  ->
      let w = I.Entry (newTemp fi sem.expr_type)
      and sem1 = interOfExpr e1
      and sem2 = interOfExpr e2 in
      add_quad (genQuad I.O_Plus sem1.place sem2.place w);
      sem.place <- w;
      func_res := sem :: !func_res;
      sem
  | E_FMinus (sem, fi, e1, e2) ->
      let w = I.Entry (newTemp fi sem.expr_type)
      and sem1 = interOfExpr e1
      and sem2 = interOfExpr e2 in
      add_quad (genQuad I.O_Minus sem1.place sem2.place w);
      sem.place <- w;
      func_res := sem :: !func_res;
      sem
  | E_FMul (sem, fi, e1, e2)   ->
      let w = I.Entry (newTemp fi sem.expr_type)
      and sem1 = interOfExpr e1
      and sem2 = interOfExpr e2 in
      add_quad (genQuad I.O_Mult sem1.place sem2.place w);
      sem.place <- w;
      func_res := sem :: !func_res;
      sem
  | E_FDiv (sem, fi, e1, e2)   ->
      let w = I.Entry (newTemp fi sem.expr_type)
      and sem1 = interOfExpr e1
      and sem2 = interOfExpr e2 in
      add_quad (genQuad I.O_Div sem1.place sem2.place w);
      sem.place <- w;
      func_res := sem :: !func_res;
      sem
  | E_Pow (sem, fi, e1, e2)    ->
      let w = I.Entry (newTemp fi sem.expr_type)
      and sem1 = interOfExpr e1
      and sem2 = interOfExpr e2 in
      add_quad (genQuad I.O_Pow sem1.place sem2.place w);
      sem.place <- w;
      func_res := sem :: !func_res;
      sem
  (* Structural and Natural Equality Operators *)
  | E_Eq (sem, fi, e1, e2)       ->
      let quad_true = nextQuad ()
      and sem1 = interOfExpr e1
      and sem2 = interOfExpr e2 in
      add_quad (genQuad I.O_SEqual sem1.place sem2.place I.Backpatch);
      let quad_false = nextQuad () in
      add_quad (genQuad I.O_Jump I.Empty I.Empty I.Backpatch);
      sem.true_ <- [quad_true];
      sem.false_ <- [quad_false];
      func_res := sem :: !func_res;
      sem
  | E_Differ (sem, fi, e1, e2)   ->
      let quad_true = nextQuad ()
      and sem1 = interOfExpr e1
      and sem2 = interOfExpr e2 in
      add_quad (genQuad I.O_SNEqual sem1.place sem2.place I.Backpatch);
      let quad_false = nextQuad () in
      add_quad (genQuad I.O_Jump I.Empty I.Empty I.Backpatch);
      sem.true_ <- [quad_true];
      sem.false_ <- [quad_false];
      func_res := sem :: !func_res;
      sem
  | E_Equal (sem, fi, e1, e2)    ->
      let quad_true = nextQuad ()
      and sem1 = interOfExpr e1
      and sem2 = interOfExpr e2 in
      add_quad (genQuad I.O_Equal sem1.place sem2.place I.Backpatch);
      let quad_false = nextQuad () in
      add_quad (genQuad I.O_Jump I.Empty I.Empty I.Backpatch);
      sem.true_ <- [quad_true];
      sem.false_ <- [quad_false];
      func_res := sem :: !func_res;
      sem
  | E_NEqual (sem, fi, e1, e2)   ->
      let quad_true = nextQuad ()
      and sem1 = interOfExpr e1
      and sem2 = interOfExpr e2 in
      add_quad (genQuad I.O_NEqual sem1.place sem2.place I.Backpatch);
      let quad_false = nextQuad () in
      add_quad (genQuad I.O_Jump I.Empty I.Empty I.Backpatch);
      sem.true_ <- [quad_true];
      sem.false_ <- [quad_false];
      func_res := sem :: !func_res;
      sem
  | E_Lt (sem, fi, e1, e2)       ->
      let quad_true = nextQuad ()
      and sem1 = interOfExpr e1
      and sem2 = interOfExpr e2 in
      add_quad (genQuad I.O_Lt sem1.place sem2.place I.Backpatch);
      let quad_false = nextQuad () in
      add_quad (genQuad I.O_Jump I.Empty I.Empty I.Backpatch);
      sem.true_ <- [quad_true];
      sem.false_ <- [quad_false];
      func_res := sem :: !func_res;
      sem
  | E_Gt (sem, fi, e1, e2)       ->
      let quad_true = nextQuad ()
      and sem1 = interOfExpr e1
      and sem2 = interOfExpr e2 in
      add_quad (genQuad I.O_Gt sem1.place sem2.place I.Backpatch);
      let quad_false = nextQuad () in
      add_quad (genQuad I.O_Jump I.Empty I.Empty I.Backpatch);
      sem.true_ <- [quad_true];
      sem.false_ <- [quad_false];
      func_res := sem :: !func_res;
      sem
  | E_Leq (sem, fi, e1, e2)      ->
      let quad_true = nextQuad ()
      and sem1 = interOfExpr e1
      and sem2 = interOfExpr e2 in
      add_quad (genQuad I.O_Leq sem1.place sem2.place I.Backpatch);
      let quad_false = nextQuad () in
      add_quad (genQuad I.O_Jump I.Empty I.Empty I.Backpatch);
      sem.true_ <- [quad_true];
      sem.false_ <- [quad_false];
      func_res := sem :: !func_res;
      sem
  | E_Geq (sem, fi, e1, e2)      ->
      let quad_true = nextQuad ()
      and sem1 = interOfExpr e1
      and sem2 = interOfExpr e2 in
      add_quad (genQuad I.O_Geq sem1.place sem2.place I.Backpatch);
      let quad_false = nextQuad () in
      add_quad (genQuad I.O_Jump I.Empty I.Empty I.Backpatch);
      sem.true_ <- [quad_true];
      sem.false_ <- [quad_false];
      func_res := sem :: !func_res;
      sem
  (* Logical Operators *)
  | E_Not (sem, fi, e)           ->
      let inter_e = interOfExpr e in
      sem.true_ <- inter_e.false_;
      sem.false_ <- inter_e.true_;
      func_res := sem :: !func_res;
      sem
  | E_Andlogic (sem, fi, e1, e2) ->
      let sem1 = interOfExpr e1 in
      backpatch sem1.true_ (nextQuad ());
      let sem2 = interOfExpr e2 in
      sem.true_ <- sem2.true_;
      sem.false_ <- merge [sem1.false_; sem2.false_];
      func_res := sem :: !func_res;
      sem
  | E_Orlogic (sem, fi, e1, e2)  ->
      let sem1 = interOfExpr e1 in
      backpatch sem1.false_ (nextQuad ());
      let sem2 = interOfExpr e2 in
      sem.true_ <- merge [sem1.true_; sem2.true_];
      sem.false_ <- sem2.false_;
      func_res := sem :: !func_res;
      sem
  (* Imperative Commands *)
  | E_Block (sem, fi, e)     -> sem
  | E_Semicolon (sem, fi, e1, e2)  ->
      Pervasives.ignore (interOfExpr e1);
      sem.place <- (interOfExpr e2).place;
      func_res := sem :: !func_res;
      sem
  | E_While (sem, fi, e1, e2)      ->
      let q = nextQuad () in
      backpatch (interOfExpr e1).true_ (nextQuad ());
      backpatch (interOfExpr e2).next q;
      add_quad (genQuad I.O_Jump I.Empty I.Empty (I.Label q));
      sem.next <- (interOfExpr e1).false_;
      func_res := sem :: !func_res;
      sem
  | E_For (sem, fi,ti, e1, e2, e) -> (* FIXME: is it correct? *)
      let cond_q = nextQuad ()
      and cond_true = (interOfExpr e2).true_
      and cond_false = (interOfExpr e2).false_
      and simple_quad = nextQuad ()
      and body_fst_quad = nextQuad () + 1 in
      add_quad (genQuad I.O_Jump I.Empty I.Empty (I.Label cond_q));
      backpatch cond_true body_fst_quad;
      add_quad (genQuad I.O_Jump I.Empty I.Empty (I.Label simple_quad));
      sem.next <- cond_false;
      func_res := sem :: !func_res;
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
          add_quad (genQuad I.O_Jump I.Empty I.Empty I.Backpatch);
          backpatch cond_sem.false_ (nextQuad ());
          let l2 = stmt2_sem.next in
          sem.next <- merge [l1; stmt1_sem.next; l2];
          func_res := sem :: !func_res;
          sem
      | None    -> (* no else stmt *)
          sem.next <- merge [l1; stmt1_sem.next; l2];
          func_res := sem :: !func_res;
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
  | P_LitId (sem, fi)           -> sem
  | P_LitChar (sem, fi, c)      -> sem
  | P_LitFloat (sem, fi, f)     -> sem
  | P_Plus (sem, fi, _)         -> sem
  | P_FPlus (sem, fi, _)        -> sem
  | P_Minus (sem, fi, _)        -> sem
  | P_FMinus  (sem, fi, _)      -> sem
  | P_LitConstr (sem, fi, patl) -> (* TODO: not supported yet *)
      error fi 3 "user defined data types are not supported"
  | _                           -> err "Wrong pattern form"
;;
