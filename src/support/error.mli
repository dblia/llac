(* Error handling *)

(* An exception raised by the low-level error printer; exported here so that it
 * can be caught in module Main and converted into an exit status for the whole
 * program. *)
exception Exit of int

(* An element of the type finfo represents a "file_position", a file name,
 * line number and the character position within the line 
 *
 * Used for printing error messages, hold in ast structure *)
type finfo
val dummyinfo : finfo

(* A convenient datatype for a 'value with info' *)
type 'a withinfo = {i: finfo; v: 'a}

(* Creates file_position info: filename, line, char *)
val createInfo : string -> int -> int -> finfo
val printFileInfo : finfo -> unit 

(* Print an error message and fail.  The printing function is called
   in a context where the formatter is processing an hvbox.  Insert
   calls to Format.print_space to print a space or, if necessary,
   break the line at that point. *)
val errf   : (unit -> unit) -> int -> 'a
val errfAt : finfo -> (unit -> unit) -> int -> 'a

(* Convenient wrappers for the above, for the common case where the
   action to be performed is just to print a given string. *)
val err   : string -> 'a
val error : finfo -> int -> string -> 'a

(* Functions that print a message without failing afterwards *)
val warning : finfo -> string -> unit

(* ------------------------------------------------------------------------- *)

exception Terminate

type verbose = Vquiet | Vnormal | Vverbose

val flagVerbose      : verbose ref

val numErrors        : int ref
val maxErrors        : int ref
val flagWarnings     : bool ref
val numWarnings      : int ref
val maxWarnings      : int ref

val internal_raw     : (string * int) ->
                         ('a, Format.formatter, unit) format -> 'a
val fatal            : ('a, Format.formatter, unit) format -> 'a
val error2           : ('a, Format.formatter, unit) format -> 'a
val warning2         : ('a, Format.formatter, unit) format -> 'a
val message          : ('a, Format.formatter, unit) format -> 'a

type position =
    POS_Point   of Lexing.position
  | POS_Context of Lexing.position * Lexing.position
  | POS_Dummy

val position_context : Lexing.position -> Lexing.position -> position
val position_dummy   : position
val print_position   : Format.formatter -> position -> unit
