(* The standard prelude implemented in OCaml *)

let unSome o =
  match o with
  | Some x -> x
  | None   -> raise (Failure "uninitialized variable")

let incr x = x := Some (unSome !x + 1)

let decr x = x := Some (unSome !x - 1)

let fabs x = abs_float x

let round x =
  if x > 0.0 then
    int_of_float (floor (x +. 0.5))
  else
    int_of_float (ceil (x -. 0.5))

let strlen s =
  let rec loop i =
     if unSome !((snd s).(i)) = '\000' then
       i
     else
       loop (i+1) in
  loop 0

let strcmp s1 s2 =
  let rec loop i =
    let c1 = unSome !((snd s1).(i)) in
    let c2 = unSome !((snd s2).(i)) in
    if c1 > c2 then
      1
    else if c1 < c2 then
      -1
    else if c1 = '\000' then
      0
    else
      loop (i+1) in
  loop 0

let strcpy s1 s2 =
  let len1 = List.hd (fst s1) in
  let len2 = strlen s2 in
  if len1 <= len2 then
    raise (Failure "string length too short")
  else
  begin
    let rec loop i =
      let c = unSome !((snd s2).(i)) in
      (snd s1).(i) := Some c;
      if c <> '\000' then
        loop (i+1) in
    loop 0
  end

let print_bool b =
  if b then print_string "true" else print_string "false"

let print_string s =
  let rec loop i =
    let c = unSome !((snd s).(i)) in
    if c <> '\000' then
    begin
       print_char c;
       loop (i+1)
    end in
  loop 0

let llamastr_of_string s =
  let l = String.length s in
  let chr n =
    if n < l then
      ref (Some (s.[n]))
    else if n == l then
      ref (Some '\000')
    else
      ref None in
  [l+1], Array.init (l+1) chr

let read_string s =
  let str  = read_line () in
  let len1 = List.hd (fst s) in
  let len2 = String.length str in
  if len1 <= len2 then
    raise (Failure "string length too short")
  else
  begin
    for i = 0 to len2 - 1 do
      let c = String.get str i in
      (snd s).(i) := Some c
    done;
    (snd s).(len2) := Some '\000'
  end

let read_char () =
  input_char stdin

let strcat s1 s2 = 
  let len1 = strlen s1 in
  let len2 = strlen s2 in
  let len_available = List.hd (fst s1) in
  if len_available <= len1 + len2 then
    raise (Failure "string length too short")
  else
  begin
    let rec loop i =
      let c = unSome !((snd s2).(i)) in
      (snd s1).(len1 + i) := Some c;
      if c <> '\000' then
        loop (i+1) in
    loop 0
  end

let ln x = 
  log x /. log 2.0

let pi () = 3.14159

(* Automatically generated stuff *)

(* The program *)

let main : unit =
  print_string (llamastr_of_string "Give n: ");
  let n : int = read_int () in
  print_string (llamastr_of_string "Give k: ");
  let k : int = read_int () in
  let sum : float option ref = ref None
  and seed : int option ref = ref None in
  sum := Some 0.;
  seed := Some 65;
  for i = 1 to
    k do
    seed := Some ((unSome !seed * 137 + 220 + i) mod n);
    sum := Some (unSome !sum +. float_of_int (unSome !seed))
  done;
  if k > 0 then
    (print_string (llamastr_of_string "Mean: ");
     print_float (unSome !sum /. float_of_int k);
     print_string (llamastr_of_string "\n"))
