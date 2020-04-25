
(* The type of tokens. *)

type token = 
  | RIGHTBRAKET
  | RIGHT
  | MOVE
  | LEFTBRAKET
  | LEFT
  | FUNC of (string)
  | EOF
  | ENDDEF
  | DEFINE
  | COLOR of (int)

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.program)
