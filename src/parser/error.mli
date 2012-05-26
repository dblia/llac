(* Error handling *)

exception Terminate

val numErrors   : int ref   (* Total erros *)

type position =
  POS_Point of Lexing.position

val print_position : Format.formatter -> position -> unit
val message        : ('a, Format.formatter, unit) format -> 'a
