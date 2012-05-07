open Option

(*
and abst_expr =
  (* Terminal Symbols  *) 
  | Unit
  | True
  | False
  | Int_Const of int
  | Float_Const of float
  | Char_Const of char
  (* Unary expressions *)
  | Uplus of abst_expr
  | Uminus of abst_expr
  | UFplus of abst_expr
  | UFminus of abst_expr
  | Unref of abst_expr
  | Not of abst_expr
  (* Binary abst_expressions *)
  | Plus of abst_expr * abst_expr
  | Minus of abst_expr * abst_expr 
  | Times of abst_expr * abst_expr
  | Div of abst_expr * abst_expr
  | FPlus of abst_expr * abst_expr
  | FMinus of abst_expr * abst_expr 
  | FTimes of abst_expr * abst_expr
  | FDiv of abst_expr * abst_expr
  | Mod of abst_expr * abst_expr
  | Pow of abst_expr * abst_expr
  | Eq of abst_expr * abst_expr
  | Differ of abst_expr * abst_expr
  | Lt of abst_expr * abst_expr
  | Gt of abst_expr * abst_expr
  | Le of abst_expr * abst_expr
  | Ge of abst_expr * abst_expr
  | Equal of abst_expr * abst_expr
  | NEqual of abst_expr * abst_expr
  | Andlogic of abst_expr * abst_expr
  | Orlogic of abst_expr * abst_expr
  | Sequence of abst_expr * abst_expr
  | Assign of abst_expr * abst_expr
;;

let rec pp_tree out t = 
  match t with
    | Unit              -> fprintf out "()"
    | True              -> fprintf out "True"
    | False             -> fprintf out "False"
    | Int_Const n       -> fprintf out "%d" n
    | Float_Const n     -> fprintf out "%f" n
    | Char_Const c      -> fprintf out "%c" c
    | Uplus t1          -> fprintf out "Uplus(%a)" (pp_tree t1)
    | Uminus t1         -> fprintf out "Uminus(%a)" (pp_tree t1)
    | UFplus t1         -> fprintf out "UFplus(%a" (pp_tree t1)
    | UFminus t1        -> fprintf out "UFminus(%a)" (pp_tree t1)
    | Unref t1          -> fprintf out "Unref(%a)" (pp_tree t1) 
    | Not t1            -> fprintf out "Not(%a)" (pp_tree t1)
    | Plus (t1, t2)     -> fprintf out "Plus(%a, %a)" (pp_tree t1) (pp_tree t2)
    | Minus (t1, t2)    -> fprintf out "Minus(%a, %a)" (pp_tree t1) (pp_tree t2)
    | Times (t1, t2)    -> fprintf out "Times(%a, %a)" (pp_tree t1) (pp_tree t2)
    | Div (t1, t2)      -> fprintf out "Div(%a, %a)" (pp_tree t1) (pp_tree t2)
    | FPlus (t1, t2)    -> fprintf out "FPlus(%a, %a)" (pp_tree t1) (pp_tree t2)
    | FMinus (t1, t2)   -> fprintf out "FMinus(%a, %a)" (pp_tree t1) (pp_tree t2)
    | FTimes (t1, t2)   -> fprintf out "FTimes(%a, %a)" (pp_tree t1) (pp_tree t2)
    | FDiv (t1, t2)     -> fprintf out "FDiv(%a, %a)" (pp_tree t1) (pp_tree t2)
    | Mod (t1, t2)      -> fprintf out "Mod(%a, %a)" (pp_tree t1) (pp_tree t2) 
    | Pow (t1, t2)      -> fprintf out "Pow(%a, %a)" (pp_tree t1) (pp_tree t2)
    | Eq (t1, t2)       -> fprintf out "Eq(%a, %a)" (pp_tree t1) (pp_tree t2) 
    | Differ (t1, t2)   -> fprintf out "Differ(%a, %a)" (pp_tree t1) (pp_tree t2)
    | Lt (t1, t2)       -> fprintf out "Lt(%a, %a)" (pp_tree t1) (pp_tree t2)
    | Gt (t1, t2)       -> fprintf out "Gt(%a, %a)" (pp_tree t1) (pp_tree t2)  
    | Le (t1, t2)       -> fprintf out "Le(%a, %a)" (pp_tree t1) (pp_tree t2)
    | Ge (t1, t2)       -> fprintf out "Ge(%a, %a)" (pp_tree t1) (pp_tree t2)
    | Equal (t1, t2)    -> fprintf out "Equal(%a, %a)" (pp_tree t1) (pp_tree t2)
    | NEqual (t1, t2)   -> fprintf out "NEqual(%a, %a)" (pp_tree t1) (pp_tree t2)
    | Andlogic (t1, t2) -> fprintf out "Andlogic(%a, %a)" (pp_tree t1) (pp_tree t2)
    | Orlogic (t1, t2)  -> fprintf out "Orlogic(%a, %a)" (pp_tree t1) (pp_tree t2)
    | Assign (t1, t2)   -> fprintf out "Assign(%a, %a)" (pp_tree t1) (pp_tree t2) 
;;

*)
