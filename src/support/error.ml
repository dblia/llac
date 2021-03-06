open Format
open Lexing
open Identifier

(* Taken by Benjamin Pierce tyarith intepreter: <link>  *)

exception Exit of int

let numErrors = ref 0
let maxErrors = ref 10

type finfo = FI of string * int * int | UNKNOWN
type 'a withinfo = {i: finfo; v: 'a}

let dummyinfo = UNKNOWN
let createInfo name line chr = FI (name, line, chr)

let printFileInfo = function
    FI (name, line, chr) ->
      print_string "Error: file \"";
      print_string name;
      print_string "\", line ";
      print_int line;
      print_string ", character ";
      print_int chr;
      print_string ":"
  | UNKNOWN ->
      print_string "<Unknown file and line>: "

let errf f num =
  print_flush();
  f();
  print_newline();
  if num == 0 then exit 1
  else raise (Exit num)

let errfAt fi f num = errf (fun() -> printFileInfo fi; print_space(); f()) num

let err s =
  print_flush();
  print_string s;
  print_newline();
  exit 1

let error fi num s = errfAt fi (fun() -> print_string s) num

let error_args fi s arg =
  incr numErrors;
  print_flush();
  printFileInfo fi;
  print_space();
  print_string s;
  printf " %a " pretty_id arg;
  print_newline()

let warning fi s =
  printFileInfo fi;
  print_string "Warning: ";
  print_string s

(* ------------------------------------------------------------------------- *)

exception Terminate

type verbose = Vquiet | Vnormal | Vverbose

let flagVerbose = ref Vnormal

let flagWarnings = ref true
let numWarnings = ref 0
let maxWarnings = ref 200

type position =
    POS_Point   of Lexing.position
  | POS_Context of Lexing.position * Lexing.position
  | POS_Dummy

let position_point lpos = POS_Point lpos
let position_context lpos_start lpos_end = POS_Context (lpos_start, lpos_end)
let position_dummy = POS_Dummy

let print_position ppf pos =
  match pos with
  | POS_Point lpos ->
      fprintf ppf "@[file \"%s\",@ line %d,@ character %d:@]@ "
        lpos.pos_fname lpos.pos_lnum (lpos.pos_cnum - lpos.pos_bol)
  | POS_Context (lpos_start, lpos_end) ->
      if lpos_start.pos_fname != lpos_end.pos_fname then
        fprintf ppf "@[file \"%s\",@ line %d,@ character %d to@ \
                     file %s,@ line %d,@ character %d:@]@ "
          lpos_start.pos_fname lpos_start.pos_lnum
          (lpos_start.pos_cnum - lpos_start.pos_bol)
          lpos_end.pos_fname lpos_end.pos_lnum
          (lpos_end.pos_cnum - lpos_end.pos_bol)
      else if lpos_start.pos_lnum != lpos_end.pos_lnum then
        fprintf ppf "@[file \"%s\",@ line %d,@ character %d to@ \
                     line %d,@ character %d:@]@ "
          lpos_start.pos_fname lpos_start.pos_lnum
          (lpos_start.pos_cnum - lpos_start.pos_bol)
          lpos_end.pos_lnum
          (lpos_end.pos_cnum - lpos_end.pos_bol)
      else if lpos_start.pos_cnum - lpos_start.pos_bol !=
              lpos_end.pos_cnum - lpos_end.pos_bol then
        fprintf ppf "@[file \"%s\",@ line %d,@ characters %d to %d:@]@ "
          lpos_start.pos_fname lpos_start.pos_lnum
          (lpos_start.pos_cnum - lpos_start.pos_bol)
          (lpos_end.pos_cnum - lpos_end.pos_bol)
      else
        fprintf ppf "@[file \"%s\", line %d, character %d:@]@ "
          lpos_start.pos_fname lpos_start.pos_lnum
          (lpos_start.pos_cnum - lpos_start.pos_bol)
  | POS_Dummy ->
      ()

let no_out buf pos len = ()
let no_flush () = ()
let null_formatter = make_formatter no_out no_flush

let internal_raw (fname, lnum) fmt =
  let fmt = "@[<v 2>" ^^ fmt ^^ "@]@;@?" in
  incr numErrors;
  let cont ppf =
    raise Terminate in
  eprintf "Internal error occurred at %s:%d,@ " fname lnum;
  kfprintf cont err_formatter fmt

and fatal fmt =
  let fmt = "@[<v 2>Fatal error: " ^^ fmt ^^ "@]@;@?" in
  incr numErrors;
  let cont ppf =
    raise Terminate in
  kfprintf cont err_formatter fmt

and error2 fmt =
  let fmt = "@[<v 2>Error: " ^^ fmt ^^ "@]@;@?" in
  incr numErrors;
  if !numErrors >= !maxErrors then
    let cont ppf =
      eprintf "Too many errors, aborting...\n";
      raise Terminate in
    kfprintf cont err_formatter fmt
  else
    eprintf fmt

and warning2 fmt =
  let fmt = "@[<v 2>Warning: " ^^ fmt ^^ "@]@;@?" in
  if !flagWarnings then
  begin
    incr numWarnings;
    if !numWarnings >= !maxWarnings then
      let cont ppf =
        eprintf "Too many warnings, no more will be shown...\n";
        flagWarnings := false in
      kfprintf cont err_formatter fmt
    else
      eprintf fmt
  end
  else
    fprintf null_formatter fmt

and message fmt =
  let fmt = "@[<v 2>" ^^ fmt ^^ "@]@;@?" in
  eprintf fmt
